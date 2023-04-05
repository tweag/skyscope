{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Query where

import Common
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.HList (Label (..))
import Data.HList.Record (HasField, hLookupByLabel)
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import Database.SQLite3 (SQLData (..))
import Foreign.C.Types (CInt (..), CLong (..))
import qualified Foreign.Marshal.Array as Marshal
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf)
import GHC.Generics (Generic)
import Model
import Prelude
import qualified Sqlite
import Sqlite (Database)

type Pattern = Text

data QueryResult = QueryResult
  { resultTotalNodes :: Int,
    resultNodes :: NodeMap Node
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type FindPathMemo = TVar (Map (NodeHash, NodeHash) [NodeHash])

type HasFindPathMemo r = HasField "findPath" r FindPathMemo

getFindPathMemo :: HasFindPathMemo r => r -> FindPathMemo
getFindPathMemo = hLookupByLabel (Label :: Label "findPath")

findPath ::
  HasFindPathMemo r =>
  Database ->
  NodeHash ->
  NodeHash ->
  Memoize r [NodeHash]
findPath database = curry $
  memoize "findPath" getFindPathMemo $ \(origin, destination) -> liftIO $ do
    destination <- getNodeIdx destination
    origin <- getNodeIdx origin
    steps <-
      Sqlite.executeSql
        database
        ["SELECT steps FROM path WHERE destination = ?;"]
        [SQLInteger destination]
    case steps of
      [] -> error $ "no path data for " <> show destination
      [[SQLBlob steps]] -> do
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
      [_] -> error "steps column has unexpected data type"
      _ : _ : _ -> error "should be impossible due to primary key constraint on destination column"
  where
    getNodeIdx :: Text -> IO Int64
    getNodeIdx hash =
      Sqlite.executeSqlScalar
        database
        ["SELECT idx FROM node WHERE hash = ?;"]
        [SQLText hash]
        <&> \(SQLInteger n) -> n

foreign import ccall safe "path.cpp"
  c_findPath ::
    CInt -> -- origin
    CInt -> -- destination
    Ptr CLong -> -- stepMap
    CInt -> -- stepMapSize
    Ptr CInt -> -- pathBuffer
    CInt -> -- maxLength
    IO CInt

type FloodNodesMemo = TVar (Map (NodeHash, Pattern, Set NodeType) QueryResult)

type HasFloodNodesMemo r = HasField "floodNodes" r FloodNodesMemo

getFloodNodesMemo :: HasFloodNodesMemo r => r -> FloodNodesMemo
getFloodNodesMemo = hLookupByLabel (Label :: Label "floodNodes")

floodNodes ::
  HasFloodNodesMemo r =>
  Database ->
  Int ->
  NodeHash ->
  Pattern ->
  Set NodeType ->
  Memoize r QueryResult
floodNodes _database _limit _source _pattern _types = error "not implemented"

type FilterNodesMemo = TVar (Map Pattern QueryResult)

type HasFilterNodesMemo r = HasField "filterNodes" r FilterNodesMemo

getFilterNodesMemo :: HasFilterNodesMemo r => r -> FilterNodesMemo
getFilterNodesMemo = hLookupByLabel (Label :: Label "filterNodes")

filterNodes ::
  HasFilterNodesMemo r =>
  Database ->
  Int64 ->
  Pattern ->
  Memoize r QueryResult
filterNodes database limit = memoize "filterNodes" getFilterNodesMemo $ \pattern -> do
  SQLInteger total <-
    liftIO $
      Sqlite.executeSqlScalar
        database
        ["SELECT COUNT(hash) FROM node WHERE data LIKE ?"]
        [SQLText pattern]
  records <-
    liftIO $
      Sqlite.executeSql
        database
        ["SELECT hash, data, type FROM node WHERE data LIKE ? LIMIT ?;"]
        [SQLText pattern, SQLInteger limit]
  pure . QueryResult (fromIntegral total) $
    Map.fromList $
      records <&> \[SQLText hash, SQLText nodeData, SQLText nodeType] ->
        (hash, Node nodeData nodeType)

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
    projectEdge [ _, SQLText source, SQLText target ] = [ source, target ]
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
