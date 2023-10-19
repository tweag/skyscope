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

import Common (getDataDirectory, getSkyscopeEnv)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar, newTVar, newTVarIO, readTVar)
import Control.Exception (handle, try, tryJust)
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Json
import Data.Bitraversable (bitraverse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.ByteUnits (ByteUnit (..), ByteValue (..), getAppropriateUnits, getShortHand)
import Data.Either (fromRight)
import Data.FileEmbed (embedFile, embedFileIfExists)
import Data.Functor (void, (<&>))
import Data.HList (Label (..))
import Data.HList.Record (emptyRecord, (.*.), (.=.))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
import System.Directory (removeFile)
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
  dataDir <- getDataDirectory
  let pidFile = dataDir <> "/server.pid"
  writeFile pidFile =<< show <$> getProcessID
  withFile (dataDir <> "/server.log") AppendMode $ \logHandle -> do
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
        makePathFinderMemo <- newTVar Map.empty
        floodNodesMemo <- newTVar Map.empty
        filterNodesMemo <- newTVar Map.empty
        getNeighboursMemo <- newTVar Map.empty
        getContextMemo <- newTVar Map.empty
        unifyComponentsMemo <- newTVar Map.empty
        pure $
          ((Label :: Label "makePathFinder") .=. makePathFinderMemo)
            .*. ((Label :: Label "floodNodes") .=. floodNodesMemo)
            .*. ((Label :: Label "filterNodes") .=. filterNodesMemo)
            .*. ((Label :: Label "getNeighbours") .=. getNeighboursMemo)
            .*. ((Label :: Label "getContext") .=. getContextMemo)
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
        Right (tag, path) -> Web.json =<< do
          Import{..} <- fromJust <$> liftIO (addImport importDatabase tag path)
          void $ withMemo importId $ Query.findPath importPath "" ""  -- Cause predMap to be computed and cached
          pure Import{..}
        Left err -> badRequest err

    Web.get "/" $
      liftIO (listImports importDatabase) >>= \imports -> do
        indexJs <- liftIO indexJs
        html ("<script>" <> indexJs <> "</script>") $
          Text.unlines
            [ "<div id=\"Imports\">",
              Text.unlines $
                imports <&> \Import {..} ->
                  let importIdText = Text.pack $ show importId
                      anchor content = "<a href=\"" <> importIdText <> "\">" <> content <> "</a>"
                      span field content = "<span class=\"" <> field <> "\">" <> content <> "</span>"
                   in "<div id=\"" <> importIdText <> "\" class=\"Import\">"
                        <> Text.concat
                          [ span "Created" $ Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" importCreated,
                            span "Path" $ anchor $ Text.pack importPath,
                            span "Tag" $ anchor $ importTag,
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
                            "<input type=\"checkbox\" class=\"Delete\">"
                          ]
                        <> "</div>",
              "  <div class=\"Button\"><input id=\"button\" type=\"button\" value=\"Select All\"></div>",
              "</div>"
            ]

    Web.delete "/:importId/" $
      readMaybe <$> Web.param "importId" >>= \case
        Just importId -> Web.json =<< liftIO (deleteImport importDatabase importId)
        Nothing -> badRequest "invalid import id"

    Web.get "/:importId/" $ do
      Import {..} <- importRoute importDatabase $ const . pure
      let importIdText = Text.pack $ show importId
      mainJs <- liftIO $ mainJs importIdText importTag
      html ("<script>" <> mainJs <> "</script>") ""

    Web.post "/:importId/path" $
      Json.eitherDecode <$> Web.body >>= \case
        Right (origin, destination) ->
          let route Import {..} _ = withMemo importId $ Query.findPath importPath origin destination
           in Web.json =<< importRoute importDatabase route
        Left err -> badRequest err

    Web.post "/:importId/flood" $
      Json.eitherDecode <$> Web.body >>= \case
        Right (source, pattern, types) ->
          let route Import {..} database = withMemo importId $ Query.floodNodes database 256 source pattern types
           in Web.json =<< importRoute importDatabase route
        Left err -> badRequest err

    Web.post "/:importId/filter" $
      Json.eitherDecode <$> Web.body >>= \case
        Right pattern ->
          let route Import {..} database = withMemo importId $ Query.filterNodes database 256 pattern
           in Web.json =<< importRoute importDatabase route
        Left err -> badRequest err

    Web.post "/:importId/neighbours" $
      Json.eitherDecode <$> Web.body >>= \case
        Right nodeHash ->
          let route Import {..} database = withMemo importId $ Query.getNeighbours database nodeHash
           in Web.json =<< importRoute importDatabase route
        Left err -> badRequest err

    Web.post "/:importId/render" $
      Json.eitherDecode <$> Web.body >>= \case
        Right nodeStates ->
          let route Import {..} database = withMemo importId $ Render.renderOutput <$> Render.renderGraph database importPath nodeStates
           in Web.text =<< importRoute importDatabase route <* Web.setHeader "Content-Type" "image/svg+xml"
        Left err -> badRequest err

    Web.post "/:importId/context" $
      Json.eitherDecode <$> Web.body >>= \case
        Right contextKeys ->
          let route Import {..} database = withMemo importId $ Query.getContext database contextKeys
           in Web.json =<< importRoute importDatabase route
        Left err -> badRequest err
  where
    importRoute :: Database -> (Import -> Database -> IO a) -> ActionM a
    importRoute importDatabase action =
      readMaybe <$> Web.param "importId" >>= \case
        Just importId ->
          liftIO (withImport importDatabase importId action) >>= \case
            Nothing -> Web.raiseStatus notFound404 $ LazyText.pack "no such import"
            Just result -> pure result
        Nothing -> error "invalid import id"

    mainJs :: Text -> Text -> IO Text
    mainJs importId importTag = do
      formatJs <- liftIO formatJs
      let scriptHeader =
            Text.unlines
              [ "const importId = '" <> importId <> "';",
                "const importTag = '" <> importTag <> "';",
                formatJs
              ]
      fmap (scriptHeader <>) $
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
          Just path -> Text.decodeUtf8 <$> BS.readFile path

    indexJs :: IO Text
    indexJs =
      getSkyscopeEnv "INDEX_JS" >>= \case
        Nothing -> pure $ Text.decodeUtf8 $(embedFile "frontend/src/index.js")
        Just path -> Text.decodeUtf8 <$> BS.readFile path

    formatJs :: IO Text
    formatJs = do
      functionBody <-
        getSkyscopeEnv "FORMAT_JS" >>= \case
          Nothing -> pure $ Text.decodeUtf8 $(embedFile "frontend/src/format.js")
          Just path -> Text.decodeUtf8 <$> BS.readFile path
      pure $ "function _formatNodeContent(node) { " <> functionBody <> " };"

    themeCss :: IO Text
    themeCss =
      getSkyscopeEnv "THEME_CSS" >>= \case
        Nothing -> pure $ Text.decodeUtf8 $(embedFile "frontend/src/theme.css")
        Just path -> Text.decodeUtf8 <$> BS.readFile path

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

withImport :: Database -> UUID -> (Import -> Database -> IO a) -> IO (Maybe a)
withImport importDatabase id action =
  getImport importDatabase id >>= \case
    Just Import {..} -> Just <$> Sqlite.withDatabase importPath (action Import {..})
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
  dir <- getDataDirectory
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
