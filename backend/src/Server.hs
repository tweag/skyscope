{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Aeson as Json
import Data.FileEmbed (embedFile, embedFileIfExists)
import Data.HList (Label(..))
import Data.HList.Record ((.*.), (.=.), emptyRecord)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Language.Haskell.TH as TH
import Network.HTTP.Types.Status (badRequest400)
import Prelude
import qualified Sqlite
import System.FilePath.Find ((~~?), filePath, find)
import System.IO (hSetEncoding, stdout, utf8)
import qualified Web.Scotty as Web

import Common (getSkyscopeEnv)
import qualified Query
import qualified Render

server :: Sqlite.Database -> IO ()
server db = do
  findPathMemo <- newTVarIO Map.empty
  floodNodesMemo <- newTVarIO Map.empty
  filterNodesMemo <- newTVarIO Map.empty
  unifyComponentsMemo <- newTVarIO Map.empty
  let withMemo action = liftIO $ runReaderT action
         $  ((Label :: Label "findPath") .=. findPathMemo)
        .*. ((Label :: Label "floodNodes") .=. floodNodesMemo)
        .*. ((Label :: Label "filterNodes") .=. filterNodesMemo)
        .*. ((Label :: Label "unifyComponents") .=. unifyComponentsMemo)
        .*. emptyRecord

  let port = 28581
  hSetEncoding stdout utf8
  putStrLn $ "\nOpen this link in your browser: " <>
    "🔭 \x1b[1;36mhttp://localhost:" <> show port <> "/\x1b[0m\n"

  Web.scotty port $ do
    Web.get "/" $ do
      indexJs <- liftIO indexJs
      themeCss <- liftIO themeCss
      Web.html $ LazyText.fromStrict $ Text.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "  <head>"
        , "    <title>Skyscope</title>"
        , "    <meta charset=\"UTF-8\">"
        , "    <link rel=\"icon\" href=\"data:image/svg+xml," <> favicon <> "\">"
        , "    <script>" <> indexJs <> "</script>"
        , "    <style>" <> themeCss <> "</style>"
        , "  </head>"
        , "  <body></body>"
        , "</html>"
        ]

    Web.post "/path" $ Json.eitherDecode <$> Web.body >>= \case
      Right (origin, destination) -> Web.json =<< do
        withMemo $ Query.findPath db origin destination
      Left err -> badRequest err

    Web.post "/flood" $ Json.eitherDecode <$> Web.body >>= \case
      Right (source, pattern, types) -> Web.json =<< do
        withMemo $ Query.floodNodes db 100 source pattern types
      Left err -> badRequest err

    Web.post "/filter" $ Json.eitherDecode <$> Web.body >>= \case
      Right pattern -> Web.json =<< do
        withMemo $ Query.filterNodes db 100 pattern
      Left err -> badRequest err

    Web.post "/render" $ Json.eitherDecode <$> Web.body >>= \case
      Right nodeStates -> Web.text =<< do
        Web.setHeader "Content-Type" "image/svg+xml"
        withMemo $ Render.renderOutput <$> Render.renderGraph db nodeStates
      Left err -> badRequest err

  where
    indexJs = getSkyscopeEnv "INDEX_JS" >>= \case
      Nothing -> pure $ Text.decodeUtf8 $ fromMaybe "" $(embedFileIfExists $(do
        found <- TH.runIO $ find (pure True) (filePath ~~? "**/frontend/index.js") "."
        pure $ TH.LitE $ TH.StringL $ case found of
          [ path ] -> path
          [] -> ""))
      Just path -> Text.readFile path

    themeCss = getSkyscopeEnv "THEME_CSS" >>= \case
      Nothing -> pure $ Text.decodeUtf8 $(embedFile "frontend/src/theme.css")
      Just path -> Text.readFile path

    badRequest = Web.raiseStatus badRequest400 . LazyText.pack
    favicon = "<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 "
      <> "100 100%22><text y=%22.9em%22 font-size=%2290%22>🔭</text></svg>"
