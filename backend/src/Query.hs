{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Query where

import Common
import Control.Category ((>>>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (onException, tryJust)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.RWS.Class
import Control.Monad.Trans.State (StateT (..), evalStateT)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.Functor (void, (<&>))
import Data.HList (Label (..))
import Data.HList.Record (HasField, hLookupByLabel)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import Database.SQLite3 (SQLData (..))
import Foreign.C.Types (CInt (..), CLong (..))
import qualified Foreign.Marshal.Array as Marshal
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf)
import GHC.Generics (Generic)
import Model
import Sqlite (Database)
import qualified Sqlite
import System.IO.Error (isDoesNotExistError)
import Prelude

type Pattern = Text

data FilterParams = FilterParams
  { filterPattern :: Pattern,
    filterOrigin :: Maybe NodeHash,
    filterLimit :: Int64
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data QueryResult = QueryResult
  { resultTotalNodes :: Int64,
    resultNodes :: NodeMap Node
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type PathFinder = NodeHash -> NodeHash -> IO (MVar [NodeHash])

type MakePathFinderMemo = TVar (Map FilePath PathFinder)

type HasMakePathFinderMemo r = HasField "makePathFinder" r MakePathFinderMemo

getMakePathFinderMemo :: HasMakePathFinderMemo r => r -> MakePathFinderMemo
getMakePathFinderMemo = hLookupByLabel (Label :: Label "makePathFinder")

findPath :: HasMakePathFinderMemo r => FilePath -> NodeHash -> NodeHash -> Memoize r [NodeHash]
findPath dbPath origin destination = liftIO . readMVar =<< findPathAsync dbPath origin destination

findPathAsync :: HasMakePathFinderMemo r => FilePath -> NodeHash -> NodeHash -> Memoize r (MVar [NodeHash])
findPathAsync dbPath origin destination = do
  pathFinder <- makePathFinder dbPath
  liftIO $ pathFinder origin destination

makePathFinder :: HasMakePathFinderMemo r => FilePath -> Memoize r PathFinder
makePathFinder = memoize "makePathFinder" getMakePathFinderMemo $ \dbPath -> liftIO $ do
  (nodeCount, predMapPtr, stepMapPtr) <- Sqlite.withDatabase dbPath $ \database -> do
    void $ Sqlite.executeSql database ["DELETE FROM path;"] []
    nodeCount <- Sqlite.executeSqlScalar database ["SELECT COUNT(idx) FROM node;"] [] <&> fromSQLInt
    let predMapFile = dbPath <> ".predMap"
    predMap <-
      tryJust (guard . isDoesNotExistError) (read <$> readFile predMapFile) >>= \case
        Right predMap -> pure predMap
        Left _ -> do
          predMap <- makePredMap database nodeCount
          writeFile predMapFile $ show predMap
          pure predMap
    let predMapSize = length predMap
    predMapPtr <- Marshal.mallocArray predMapSize
    Marshal.pokeArray predMapPtr $ fromIntegral <$> predMap
    stepMapPtr <- Marshal.mallocArray nodeCount
    pure (nodeCount, predMapPtr, stepMapPtr)

  let findPath :: NodeHash -> NodeHash -> IO [NodeHash]
      findPath origin destination = Sqlite.withDatabase dbPath $ \database -> do
        let getNodeIdx :: NodeHash -> IO Int64
            getNodeIdx hash = do
              Sqlite.executeSqlScalar
                database
                ["SELECT idx FROM node WHERE hash = ?;"]
                [SQLText hash]
                <&> \(SQLInteger n) -> n
        origin <- getNodeIdx origin
        destination <- getNodeIdx destination
        steps <-
          Sqlite.executeSql
            database
            ["SELECT steps FROM path WHERE destination = ?;"]
            [SQLInteger destination]
            >>= \case
              [[SQLBlob steps]] -> pure steps
              [_] -> error "steps column has unexpected data type"
              _ : _ : _ -> error "should be impossible due to primary key constraint on destination column"
              [] -> do
                -- Path data not found in database, so compute it now.
                stepMapSize <-
                  Query.c_indexPaths
                    predMapPtr
                    (fromIntegral destination)
                    (fromIntegral nodeCount)
                    stepMapPtr
                let stepMapSizeBytes = fromIntegral stepMapSize * sizeOf (0 :: CLong)
                stepMapBytes <- Marshal.peekArray stepMapSizeBytes $ castPtr stepMapPtr
                let steps = BS.pack stepMapBytes
                steps
                  <$ Sqlite.executeSql
                    database
                    ["INSERT INTO path (destination, steps) VALUES (?, ?);"]
                    [SQLInteger $ fromIntegral destination, SQLBlob steps]

        let stepMapBytes = BS.unpack steps
            stepMapSizeBytes = length stepMapBytes
            stepMapSize = stepMapSizeBytes `div` sizeOf (0 :: CLong)
        if stepMapSize * sizeOf (0 :: CLong) /= stepMapSizeBytes
          then error $ "misaligned path data for " <> show destination
          else Marshal.allocaArray stepMapSize $ \stepMapPtr -> do
            Marshal.pokeArray (castPtr stepMapPtr) stepMapBytes
            let maxLength = 1048576 -- 4MiB of int32_t
            Marshal.allocaArray maxLength $ \pathPtr -> do
              actualLength <-
                fromIntegral
                  <$> Query.c_findPath
                    (fromIntegral origin)
                    (fromIntegral destination)
                    stepMapPtr
                    (fromIntegral stepMapSize)
                    pathPtr
                    (fromIntegral maxLength)
              if actualLength == -1
                then error "exceeded max path length"
                else do
                  path <- Marshal.peekArray actualLength pathPtr
                  for (fromIntegral <$> path) $ \nodeIdx ->
                    Sqlite.executeSql
                      database
                      ["SELECT hash FROM node WHERE idx = ?;"]
                      [SQLInteger nodeIdx]
                      <&> \case
                        [] -> error $ "failed to find hash for path node " <> show nodeIdx
                        [[SQLText nodeHash]] -> nodeHash
                        _ : _ : _ -> error "should be impossible due to primary key constraint on idx column"
                        [_] -> error "hash column has unexpected data type"

  pure $ \origin destination -> do
    result <- newEmptyMVar
    void $
      forkIO $ do
        path <- findPath origin destination `onException` putMVar result []
        putMVar result path
    pure result
  where
    makePredMap :: Database -> Int -> IO [Int]
    makePredMap database nodeCount = do
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

foreign import ccall safe "path.cpp"
  c_findPath ::
    CInt -> -- origin
    CInt -> -- destination
    Ptr CLong -> -- stepMap
    CInt -> -- stepMapSize
    Ptr CInt -> -- pathBuffer
    CInt -> -- maxLength
    IO CInt

type FilterNodesMemo = TVar (Map FilterParams QueryResult)

type HasFilterNodesMemo r = HasField "filterNodes" r FilterNodesMemo

getFilterNodesMemo :: HasFilterNodesMemo r => r -> FilterNodesMemo
getFilterNodesMemo = hLookupByLabel (Label :: Label "filterNodes")

filterNodes ::
  HasFilterNodesMemo r =>
  Database ->
  FilterParams ->
  Memoize r QueryResult
filterNodes database = memoize "filterNodes" getFilterNodesMemo $ \FilterParams {..} -> case filterOrigin of
  Just origin -> do
    let bfs :: MonadIO m => Seq Int64 -> StateT (Set Int64) m [(NodeHash, Node)]
        bfs = \case
          Empty -> pure []
          idx :<| queue -> do
            visited <- gets $ Set.member idx
            if visited
              then bfs queue
              else do
                modify $ Set.insert idx

                (nodeHash, nodeData, nodeType, contextData) <-
                  liftIO $
                    Sqlite.executeSql
                      database
                      [ "SELECT hash, data, type, context_data",
                        "FROM node LEFT JOIN context",
                        "ON node.context_key = context.context_key",
                        "WHERE idx = ?;"
                      ]
                      [SQLInteger idx]
                      <&> \case
                        [[SQLText h, SQLText d, SQLText t, SQLText c]] -> (h, d, t, c)
                        [[SQLText h, SQLText d, SQLText t, SQLNull]] -> (h, d, t, "")
                        _ -> error "sql pattern match unexpectedly failed"

                edges <-
                  liftIO $
                    Sqlite.executeSql
                      database
                      [ withFilteredNodes "",
                        "SELECT source, target FROM (",
                        "    filtered_node JOIN edge",
                        "    ON filtered_node.idx = edge.source AND edge.target = ?",
                        "    OR filtered_node.idx = edge.target AND edge.source = ?",
                        ") LIMIT ?;"
                      ]
                      [SQLText filterPattern, SQLInteger idx, SQLInteger idx, SQLInteger filterLimit]
                      <&> map (\[SQLInteger s, SQLInteger t] -> (s, t))

                let neighbours = map fst edges <> map snd edges

                visitedCount <- fromIntegral <$> gets Set.size
                nodeData <- pure $ nodeData <> "\n" <> contextData
                ((nodeHash, Node {..}) :)
                  <$> if visitedCount < filterLimit
                    then bfs (queue <> Seq.fromList neighbours)
                    else pure []

    queue <-
      liftIO $
        Sqlite.executeSqlScalar
          database
          ["SELECT idx FROM node WHERE hash = ?;"]
          [SQLText origin]
          <&> (\(SQLInteger i) -> Seq.singleton i)

    resultNodes <- Map.fromList <$> evalStateT (bfs queue) Set.empty
    let resultTotalNodes = fromIntegral $ Map.size resultNodes
    pure QueryResult {..}
  Nothing -> do
    SQLInteger total <-
      liftIO $
        Sqlite.executeSqlScalar
          database
          [ withFilteredNodes "",
            "SELECT COUNT(hash) FROM filtered_node;"
          ]
          [SQLText filterPattern]

    records <-
      liftIO $
        Sqlite.executeSql
          database
          [ withFilteredNodes "LIMIT ?",
            "SELECT hash, conjoined_data, type FROM filtered_node;"
          ]
          [SQLText filterPattern, SQLInteger filterLimit]

    pure . QueryResult total $
      Map.fromList $
        records <&> \[SQLText hash, SQLText nodeData, SQLText nodeType] ->
          (hash, Node nodeData nodeType)
  where
    withFilteredNodes limit =
      Text.unlines
        [ "WITH filtered_node(idx, hash, conjoined_data, type) AS (",
          "    SELECT idx, hash, data || CHAR(10) ||",
          "    REPLACE(COALESCE(context_data, ''), CHAR(10), ' ')",
          "    AS conjoined_data, type",
          "    FROM node LEFT JOIN context ON node.context_key = context.context_key",
          "    WHERE conjoined_data LIKE ?",
          limit,
          ")"
        ]

type GetNeighboursMemo = TVar (Map NodeHash [NodeHash])

type HasGetNeighboursMemo r = HasField "getNeighbours" r GetNeighboursMemo

getGetNeighboursMemo :: HasGetNeighboursMemo r => r -> GetNeighboursMemo
getGetNeighboursMemo = hLookupByLabel (Label :: Label "getNeighbours")

getNeighbours ::
  HasGetNeighboursMemo r =>
  Database ->
  NodeHash ->
  Memoize r [NodeHash]
getNeighbours database = memoize "getNeighbours" getGetNeighboursMemo $ \nodeHash -> do
  incomingEdges <- liftIO $ selectEdges database nodeHash "t.hash = ?"
  outgoingEdges <- liftIO $ selectEdges database nodeHash "s.hash = ?"
  pure $ filter (/= nodeHash) $ sort $ nub $ concat $ projectEdge <$> (incomingEdges <> outgoingEdges)
  where
    projectEdge :: [SQLData] -> [NodeHash]
    projectEdge [_, SQLText source, SQLText target] = [source, target]
    projectEdge _ = error "sql pattern match unexpectedly failed"

type GetContextMemo = TVar (Map [Text] (Map Text Text))

type HasGetContextMemo r = HasField "getContext" r GetContextMemo

getContextMemo :: HasGetContextMemo r => r -> GetContextMemo
getContextMemo = hLookupByLabel (Label :: Label "getContext")

getContext ::
  HasGetContextMemo r => Database -> [Text] -> Memoize r (Map Text Text)
getContext database = memoize "getContext" getContextMemo $
  fmap (fmap (Map.fromList . catMaybes)) $
    traverse $ \key ->
      liftIO $
        Sqlite.executeSql
          database
          ["SELECT context_data FROM context WHERE context_key LIKE ?;"]
          [SQLText key]
          <&> \case
            [[SQLText contextData]] -> Just (key, contextData)
            _ -> Nothing

selectEdges :: Database -> NodeHash -> Text -> IO [[SQLData]]
selectEdges database nodeHash whereClause =
  Sqlite.executeSql
    database
    [ "SELECT group_num, s.hash, t.hash FROM edge",
      "INNER JOIN node AS s ON s.idx = edge.source",
      "INNER JOIN node AS t ON t.idx = edge.target",
      "WHERE " <> whereClause <> ";"
    ]
    [SQLText nodeHash]
