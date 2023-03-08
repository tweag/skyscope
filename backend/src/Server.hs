{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Server where

import Common (getSkyscopeEnv)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar, newTVar, newTVarIO, readTVar)
import Control.Exception (handle, try, tryJust)
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Json
import Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.ByteUnits (ByteUnit (..), ByteValue (..), getAppropriateUnits, getShortHand)
import Data.Either (fromRight)
import Data.FileEmbed (embedFile, embedFileIfExists)
import Data.Functor (void, (<&>))
import Data.HList (Label (..))
import Data.HList.Record (emptyRecord, (.*.), (.=.))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Traversable (for)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Database.SQLite3 (SQLData (..), SQLError (..))
import GHC.Generics (Generic)
import GHC.IO.Handle (hDuplicateTo)
import qualified Language.Haskell.TH as TH
import Network.HTTP.Types.Status (badRequest400, internalServerError500, notFound404)
import qualified Network.Wai as Web
import qualified Network.Wai.Handler.Warp as Web
import qualified Query
import qualified Render
import Sqlite (Database)
import qualified Sqlite
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getEnv)
import System.FilePath.Find (filePath, find, (~~?))
import System.IO (BufferMode (..), IOMode (..), hSetBuffering, hSetEncoding, stderr, stdout, utf8, withFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (fileSize, getFileStatus)
import System.Posix.Process (getProcessID)
import Text.Read (readMaybe)
import Web.Scotty (ActionM)
import qualified Web.Scotty as Web
import Prelude

server :: Int -> IO ()
server port = withImportDb $ \importDatabase -> do
  homeDir <- getHomeDirectory
  let pidFile = homeDir <> "/server.pid"
  writeFile pidFile =<< show <$> getProcessID
  withFile (homeDir <> "/server.log") AppendMode $ \logHandle -> do
    let redirect h = do
          hDuplicateTo logHandle h
          hSetBuffering h LineBuffering
          hSetEncoding h utf8
    redirect stdout
    redirect stderr

  memos <- newTVarIO Map.empty
  let getMemo id =
        atomically $
          Map.lookup id <$> readTVar memos >>= \case
            Just memo -> pure memo
            Nothing -> do
              memo <- newMemo
              modifyTVar memos $ Map.insert id memo
              pure memo
      newMemo = do
        findPathMemo <- newTVar Map.empty
        floodNodesMemo <- newTVar Map.empty
        filterNodesMemo <- newTVar Map.empty
        unifyComponentsMemo <- newTVar Map.empty
        pure $
          ((Label :: Label "findPath") .=. findPathMemo)
            .*. ((Label :: Label "floodNodes") .=. floodNodesMemo)
            .*. ((Label :: Label "filterNodes") .=. filterNodesMemo)
            .*. ((Label :: Label "unifyComponents") .=. unifyComponentsMemo)
            .*. emptyRecord
      withMemo id action = liftIO $ runReaderT action =<< getMemo id

  let opts =
        Web.Options
          { verbose = 0,
            settings =
              Web.setPort port $
                flip Web.setOnExceptionResponse Web.defaultSettings $
                  Web.responseLBS internalServerError500 [] . LBSC.pack . show
          }

  Web.scottyOpts opts $ do
    Web.post "/" $
      Json.eitherDecode <$> Web.body >>= \case
        Right (tag, path) -> Web.json =<< liftIO (addImport importDatabase tag path)
        Left err -> badRequest err

    Web.get "/" $
      liftIO (listImports importDatabase) >>= \imports -> do
        indexJs <- liftIO indexJs
        html ("<script>" <> indexJs <> "</script>") $
          Text.unlines
            [ "<div id=\"imports\">",
              Text.unlines $
                imports <&> \Import {..} ->
                  let importIdText = Text.pack $ show importId
                      span field value =
                        "<span class=\"import"
                          <> field
                          <> "\">"
                          <> value
                          <> "</span>"
                   in "<div id=\"" <> importIdText <> "\" class=\"import\">"
                        <> Text.concat
                          [ span "Created" $ Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" importCreated,
                            span "Path" $
                              "<a href=\"" <> importIdText
                                <> "\">"
                                <> Text.pack importPath,
                            span "Tag" importTag <> "</a>",
                            span "Counts" $ case importCounts of
                              Nothing -> "&lt;unable to open import&gt;"
                              Just Counts {..} ->
                                Text.unwords
                                  [ Text.pack $ show nodeCount,
                                    "nodes,",
                                    Text.pack $ show edgeCount,
                                    "edges"
                                  ],
                            span "Usage" $
                              Text.pack $
                                getShortHand $
                                  getAppropriateUnits $
                                    ByteValue (fromIntegral importDiskUsage) Bytes,
                            "<input type=\"checkbox\" class=\"delete\">"
                          ]
                        <> "</div>",
              "  <div class=\"button\"><input id=\"button\" type=\"button\" value=\"Select All\"></div>",
              "</div>"
            ]

    Web.delete "/:id/" $
      readMaybe <$> Web.param "id" >>= \case
        Just id -> Web.json =<< liftIO (deleteImport importDatabase id)
        Nothing -> badRequest "invalid id"

    Web.get "/:id/" $ do
      id <- importRoute importDatabase $ const . pure
      mainJs <- liftIO $ mainJs $ Text.pack $ show id
      html ("<script>" <> mainJs <> "</script>") ""

    Web.post "/:id/path" $
      Json.eitherDecode <$> Web.body >>= \case
        Right (origin, destination) ->
          Web.json
            =<< importRoute
              importDatabase
              ( \id database ->
                  withMemo id $
                    Query.findPath database origin destination
              )
        Left err -> badRequest err

    Web.post "/:id/flood" $
      Json.eitherDecode <$> Web.body >>= \case
        Right (source, pattern, types) ->
          Web.json
            =<< importRoute
              importDatabase
              ( \id database ->
                  withMemo id $
                    Query.floodNodes database 256 source pattern types
              )
        Left err -> badRequest err

    Web.post "/:id/filter" $
      Json.eitherDecode <$> Web.body >>= \case
        Right pattern ->
          Web.json
            =<< importRoute
              importDatabase
              ( \id database ->
                  withMemo id $
                    Query.filterNodes database 256 pattern
              )
        Left err -> badRequest err

    Web.post "/:id/render" $
      Json.eitherDecode <$> Web.body >>= \case
        Right nodeStates ->
          Web.text
            =<< importRoute
              importDatabase
              ( \id database ->
                  withMemo id $
                    Render.renderOutput <$> Render.renderGraph database nodeStates
              )
            <* Web.setHeader "Content-Type" "image/svg+xml"
        Left err -> badRequest err
  where
    importRoute :: Database -> (UUID -> Database -> IO a) -> ActionM a
    importRoute importDatabase action =
      readMaybe <$> Web.param "id" >>= \case
        Just id ->
          liftIO (withImport importDatabase id $ action id) >>= \case
            Nothing -> Web.raiseStatus notFound404 $ LazyText.pack "no such import"
            Just result -> pure result
        Nothing -> error "invalid import id"

    themeCss :: IO Text
    themeCss =
      getSkyscopeEnv "THEME_CSS" >>= \case
        Nothing -> pure $ Text.decodeUtf8 $(embedFile "frontend/src/theme.css")
        Just path -> Text.readFile path

    indexJs :: IO Text
    indexJs =
      getSkyscopeEnv "INDEX_JS" >>= \case
        Nothing -> pure $ Text.decodeUtf8 $(embedFile "frontend/src/index.js")
        Just path -> Text.readFile path

    mainJs :: Text -> IO Text
    mainJs importId =
      let scriptHeader = "const importId = '" <> importId <> "';"
       in fmap (scriptHeader <>) $
            getSkyscopeEnv "MAIN_JS" >>= \case
              Nothing ->
                pure $
                  Text.decodeUtf8 $
                    fromMaybe
                      ""
                      $( embedFileIfExists
                           $( do
                                found <- TH.runIO $ find (pure True) (filePath ~~? "**/frontend/main.js") "."
                                pure $
                                  TH.LitE $
                                    TH.StringL $ case found of
                                      [path] -> path
                                      [] -> ""
                                      _ -> error "unexpectedly found multiple main.js files"
                            )
                       )
              Just path -> Text.readFile path

    badRequest :: String -> ActionM a
    badRequest = Web.raiseStatus badRequest400 . LazyText.pack

    favicon :: Text
    favicon =
      "<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 "
        <> "100 100%22><text y=%22.9em%22 font-size=%2290%22>🔭</text></svg>"

    html :: Text -> Text -> ActionM ()
    html head body = do
      themeCss <- liftIO themeCss
      Web.html $
        LazyText.fromStrict $
          Text.unlines
            [ "<!DOCTYPE html>",
              "<html>",
              "  <head>",
              "    <title>Skyscope</title>",
              "    <meta charset=\"UTF-8\">",
              "    <link rel=\"icon\" href=\"data:image/svg+xml," <> favicon <> "\">",
              "    <style>" <> themeCss <> "</style>",
              head,
              "  </head>",
              "  <body>",
              body,
              "  </body>",
              "</html>"
            ]

getHomeDirectory :: IO FilePath
getHomeDirectory = do
  dir <- getEnv "HOME" <&> (<> "/.skyscope")
  createDirectoryIfMissing True dir
  pure dir

data Import = Import
  { importId :: UUID,
    importTag :: Text,
    importPath :: FilePath,
    importCreated :: UTCTime,
    importDiskUsage :: Integer,
    importCounts :: Maybe Counts
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Counts = Counts
  { nodeCount :: Int,
    edgeCount :: Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

withImport :: Database -> UUID -> (Database -> IO a) -> IO (Maybe a)
withImport importDatabase id action =
  getImport importDatabase id >>= \case
    Just Import {..} -> Just <$> Sqlite.withDatabase importPath action
    Nothing -> pure Nothing

addImport :: Database -> String -> FilePath -> IO (Maybe Import)
addImport importDatabase tag path =
  if not $ hasDbSuffix path
    then error "expected database path to have .sqlite suffix"
    else do
      id <- UUID.nextRandom
      created <- getCurrentTime
      void $
        Sqlite.executeSql
          importDatabase
          ["INSERT into import (id, tag, path, created) VALUES (?, ?, ?, ?);"]
          (SQLText . Text.pack <$> [show id, tag, path, show created])
      getImport importDatabase id

deleteImport :: Database -> UUID -> IO Bool
deleteImport importDatabase id =
  getImport importDatabase id >>= \case
    Just Import {..} ->
      if hasDbSuffix importPath
        then do
          void $ tryJust (guard . isDoesNotExistError) (removeFile importPath)
          True
            <$ Sqlite.executeSql
              importDatabase
              ["DELETE FROM import WHERE id = ?;"]
              [SQLText $ Text.pack $ show id]
        else pure False
    Nothing -> pure False

getImport :: Database -> UUID -> IO (Maybe Import)
getImport importDatabase id = do
  rows <-
    Sqlite.executeSql
      importDatabase
      ["SELECT id, tag, path, created FROM import WHERE id = ?;"]
      [SQLText $ Text.pack $ show id]
  case rows of
    [] -> pure Nothing
    [[_, SQLText tag, SQLText p, SQLText c]] -> do
      let path = Text.unpack p
      counts <-
        try @SQLError $
          Sqlite.withDatabase path getCounts
      usage <-
        tryJust (guard . isDoesNotExistError) $
          fromIntegral . fileSize <$> getFileStatus path
      pure $
        readMaybe (Text.unpack c) <&> \created ->
          Import id tag path created (fromRight 0 usage) (fromRight Nothing counts)
    _ -> error "should be impossible due to primary key constraint on id column"

getCounts :: Database -> IO (Maybe Counts)
getCounts database =
  handleSQLError $
    sequenceCounts $
      (,)
        <$> Sqlite.executeSqlScalar database ["SELECT COUNT(*) FROM node;"] []
        <*> Sqlite.executeSqlScalar database ["SELECT COUNT(*) FROM edge;"] []
        <&> join bitraverse Sqlite.fromSQLInteger
  where
    handleSQLError = handle @SQLError $ const $ pure Nothing
    sequenceCounts = fmap $ fmap $ uncurry Counts

listImports :: Database -> IO [Import]
listImports importDatabase = do
  rows <-
    Sqlite.executeSql
      importDatabase
      ["SELECT id FROM import ORDER BY created DESC;"]
      []
  fmap catMaybes $
    for rows $ \case
      [SQLText id] -> case readMaybe $ Text.unpack id of
        Just id -> getImport importDatabase id
        Nothing -> error $ "failed to parse uuid: " <> Text.unpack id
      _ -> error "did not expect multiple columns"

withImportDb :: (Database -> IO a) -> IO a
withImportDb action = do
  dir <- getHomeDirectory
  let path = dir <> "/imports.sqlite"
  Sqlite.withDatabase path $ \database -> do
    createSchema database
    action database

createSchema :: Database -> IO ()
createSchema database =
  Sqlite.executeStatements
    database
    [ [ "CREATE TABLE IF NOT EXISTS import (",
        "  id TEXT,",
        "  tag TEXT,",
        "  path TEXT,",
        "  created TEXT,",
        "  PRIMARY KEY (id),",
        "  UNIQUE (path)",
        ");"
      ]
    ]

hasDbSuffix :: FilePath -> Bool
hasDbSuffix path = case Text.stripSuffix ".sqlite" $ Text.pack path of
  Nothing -> False
  Just _ -> True
