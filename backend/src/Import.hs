{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Import where

import Common
import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.FileEmbed (embedFile)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Database.SQLite3 (SQLData (..))
import Foreign.C.String (CString, withCString)
import Sqlite (Database)
import qualified Sqlite
import Prelude

withDatabase :: String -> FilePath -> (Database -> IO a) -> IO a
withDatabase label path action = timed label $
  Sqlite.withDatabase path $ \database -> do
    putStrLn label
    createSchema database
    Sqlite.executeStatements
      database
      [ ["pragma synchronous = off;"],
        ["pragma journal_mode = MEMORY;"],
        ["pragma mmap_size = 1073741824;"]
      ]
    action database
  where
    createSchema :: Database -> IO ()
    createSchema database = do
      let sql = Text.decodeUtf8 $(embedFile "backend/src/schema.sql")
      Sqlite.executeStatements database $ Text.lines <$> Text.splitOn "\n\n" sql

addContext :: Database -> [(Text, Text)] -> IO ()
addContext database context = do
  let records = context <&> \(k, v) -> [SQLText k, SQLText v]
  Sqlite.batchInsert database "context" ["context_key", "context_data"] records

importSkyframe :: FilePath -> IO ()
importSkyframe path =
  withDatabase "importing skyframe" path $
    const $ -- withDatabase creates the schema
      --guard =<< withCString path c_importSkyframe
      undefined

--foreign import ccall safe "import.cpp"
--  c_importSkyframe :: CString -> IO Bool

importTargets :: FilePath -> IO ()
importTargets path = withDatabase "importing targets" path $ \database -> do
  workspace <- maybe "" Text.pack <$> getSkyscopeEnv "WORKSPACE"
  external <- maybe "" Text.pack <$> getSkyscopeEnv "OUTPUT_BASE" <&> (<> "/external/")
  let parseRepository text = case Text.stripPrefix ("# " <> workspace) text of
        Just text -> ("/", text)
        Nothing -> case Text.stripPrefix ("# " <> external) text of
          Just text -> first (("@" <>) >>> (<> "/")) (Text.breakOn "/" text)
          Nothing -> error $ "failed to parse target label:\n" <> Text.unpack text
      parsePackage text =
        parseRepository text & \(repo, text) -> case Text.stripPrefix "/BUILD" text of
          Just _ -> (repo <> "/", text)
          Nothing -> first (repo <>) (Text.breakOn "/BUILD" text)
      parseLabel text =
        parsePackage text & \(package, text) ->
          ((package <> ":") <>) $ fst $ findLine "  name = \"" text & Text.break (== '"')
  targets <- map (parseLabel &&& id) <$> getParagraphs
  addContext database targets

importActions :: FilePath -> IO ()
importActions path = withDatabase "importing actions" path $ \database -> do
  let parseAction paragraph = (findLine "  Target: " paragraph, paragraph)
      indexedActions group = (0 :| [1 ..]) `NonEmpty.zip` (snd <$> group)
      filterActions = filter $ \paragraph ->
        isJust $
          asum
            [ Text.stripPrefix "action " paragraph,
              Text.stripPrefix "runfiles " paragraph,
              Text.stripPrefix "BazelCppSemantics" paragraph
            ]
  groups <-
    NonEmpty.groupBy (\x y -> fst x == fst y)
      . sortOn fst
      . map parseAction
      . filterActions
      <$> getParagraphs
  addContext database $
    concat $
      groups <&> \group@((label, _) :| _) ->
        NonEmpty.toList $
          indexedActions group <&> \(index, contextData) ->
            let contextKey = label <> " " <> Text.pack (show @Integer index)
             in (contextKey, contextData)

getParagraphs :: IO [Text]
getParagraphs = filter (Text.any (/= '\n')) . Text.splitOn "\n\n" <$> Text.getContents

findLine :: Text -> Text -> Text
findLine prefix paragraph =
  let find = \case
        [] -> error $ "missing " <> Text.unpack prefix <> ":\n" <> Text.unpack paragraph
        (line : lines) -> case Text.stripPrefix prefix line of
          Nothing -> find lines
          Just text -> text
   in find $ Text.lines paragraph
