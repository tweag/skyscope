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
import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Monad (guard)
import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.RWS.Class
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Foldable (asum, for_)
import Data.Function ((&))
import Data.Functor (void, (<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sortOn, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Time.Clock as Clock
import Data.Traversable (for)
import Database.SQLite3 (SQLData (..))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CLong (..))
import qualified Foreign.Marshal.Array as Marshal
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf)
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
importSkyframe path = withDatabase "importing skyframe" path $ \database -> do
  guard =<< withCString path c_importSkyframe
  indexing <- indexPathsAsync database
  atomically $
    readTVar indexing >>= \case
      False -> pure ()
      True -> retry
  _ <- Text.length <$> Text.getContents
  pure ()

foreign import ccall safe "import.cpp"
  c_importSkyframe :: CString -> IO Bool

indexPathsAsync :: Database -> IO (TVar Bool)
indexPathsAsync database = do
  progress <- newTVarIO (0, 1)
  indexStartTime <- Clock.getCurrentTime
  void $ forkIO $ indexPaths database progress
  indexing <- newTVarIO True
  let showProgress = do
        threadDelay 100_000
        (done, total) <- readTVarIO progress
        timeTaken <- Clock.getCurrentTime <&> (`Clock.diffUTCTime` indexStartTime)
        let unitTime = Clock.nominalDiffTimeToSeconds timeTaken / fromInteger (fromIntegral $ max 1 done)
            expectedTime = ceiling $ fromInteger (fromIntegral $ total - done) * unitTime :: Integer
            ansi = if done < total then "37" else "32"
        putStrLn $
          "\x1b[1F\x1b[2K\x1b[" <> ansi <> "mindexed paths to "
            <> show done
            <> " nodes ("
            <> show total
            <> " total, "
            <> show expectedTime
            <> " seconds remaining)\x1b[0m"
        if done < total
          then showProgress
          else do
            putStrLn $ "  time taken = " <> show timeTaken <> " seconds"
            atomically $ writeTVar indexing False
  void $ forkIO showProgress
  pure indexing

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

indexPaths :: Database -> TVar (Int, Int) -> IO ()
indexPaths database progress = do
  void $ Sqlite.executeSql database ["DELETE FROM path;"] []
  nodeCount <- Sqlite.executeSqlScalar database ["SELECT COUNT(idx) FROM node;"] [] <&> fromSQLInt
  atomically $ writeTVar progress (0, nodeCount)
  predMap <- makePredMap nodeCount
  let predMapSize = length predMap
  Marshal.allocaArray predMapSize $ \predMapPtr -> do
    Marshal.pokeArray predMapPtr $ fromIntegral <$> predMap
    destinations <- newTVarIO [1 .. fromIntegral nodeCount]
    let nextDest =
          atomically $
            stateTVar destinations $
              uncons >>> \case
                Just (next, remaining) -> (Just next, remaining)
                Nothing -> (Nothing, [])
        worker stepMapPtr =
          nextDest >>= \case
            Just destination -> do
              stepMapSize <- Import.c_indexPaths predMapPtr destination (fromIntegral nodeCount) stepMapPtr
              let stepMapSizeBytes = fromIntegral stepMapSize * sizeOf (0 :: CLong)
              stepMapBytes <- Marshal.peekArray stepMapSizeBytes $ castPtr stepMapPtr
              void $
                Sqlite.executeSql
                  database
                  ["INSERT INTO path (destination, steps) VALUES (?, ?);"]
                  [SQLInteger $ fromIntegral destination, SQLBlob $ BS.pack stepMapBytes]
              atomically $ modifyTVar progress $ first (+ 1)
              worker stepMapPtr
            Nothing -> pure ()
    workerCount <- getNumCapabilities <&> (subtract 1)
    for_ [1 .. max 1 workerCount] $ const $ forkIO $ Marshal.allocaArray nodeCount worker
    let wait = atomically $ do
          remaining <- length <$> readTVar destinations
          if remaining > 0 then retry else pure ()
    wait
  where
    makePredMap :: Int -> IO [Int]
    makePredMap nodeCount = do
      predecessors <-
        Sqlite.executeSql database ["SELECT target, source FROM edge ORDER BY target;"] []
          <&> ((map $ \[t, s] -> (fromSQLInt t, [fromSQLInt s])) >>> IntMap.fromAscListWith (++))
      pure $ uncurry (++) $ evalRWS (for [0 .. nodeCount] serialisePreds) predecessors 0

    serialisePreds :: Int -> RWS (IntMap [Int]) [Int] Int Int
    serialisePreds i =
      asks (IntMap.lookup i) <&> fromMaybe [] >>= \case
        [pred] -> pure pred
        preds -> do
          let n = length preds
          tell $ n : preds
          offset <- get <* modify (+ (1 + n))
          pure $ negate offset

    fromSQLInt :: Num a => SQLData -> a
    fromSQLInt (SQLInteger n) = fromIntegral n
    fromSQLInt value = error $ "expected data type" <> show value

foreign import ccall safe "path.cpp"
  c_indexPaths ::
    Ptr CInt -> -- predMap
    CInt -> -- destination
    CInt -> -- nodeCount
    Ptr CLong -> -- stepMap
    IO CInt
