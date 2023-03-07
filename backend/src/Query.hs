{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Map (Map)
import qualified Data.Map as Map
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
import qualified Sqlite
import Prelude

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
  Sqlite.Database ->
  NodeHash ->
  NodeHash ->
  Memoize r [NodeHash]
findPath db = curry $
  memoize "findPath" getFindPathMemo $ \(origin, destination) -> liftIO $ do
    destination <- getNodeIdx destination
    origin <- getNodeIdx origin
    steps <-
      Sqlite.executeSql
        db
        [ "SELECT steps FROM path",
          "WHERE destination = ?;"
        ]
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
                      db
                      ["SELECT hash FROM node WHERE idx = ?;"]
                      [SQLInteger nodeIdx]
                      <&> \case
                        [] -> error $ "failed to find hash for path node " <> show nodeIdx
                        [[SQLText nodeHash]] -> nodeHash
  where
    getNodeIdx :: Text -> IO Int64
    getNodeIdx hash =
      Sqlite.executeSqlScalar
        db
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
  Sqlite.Database ->
  Int ->
  NodeHash ->
  Pattern ->
  Set NodeType ->
  Memoize r QueryResult
floodNodes db limit source pattern types = error "not implemented"

type FilterNodesMemo = TVar (Map Pattern QueryResult)

type HasFilterNodesMemo r = HasField "filterNodes" r FilterNodesMemo

getFilterNodesMemo :: HasFilterNodesMemo r => r -> FilterNodesMemo
getFilterNodesMemo = hLookupByLabel (Label :: Label "filterNodes")

filterNodes ::
  HasFilterNodesMemo r =>
  Sqlite.Database ->
  Int64 ->
  Pattern ->
  Memoize r QueryResult
filterNodes db limit = memoize "filterNodes" getFilterNodesMemo $ \pattern -> do
  SQLInteger total <-
    liftIO $
      Sqlite.executeSqlScalar
        db
        [ "SELECT COUNT(hash) FROM node",
          "WHERE data LIKE ?"
        ]
        [SQLText pattern]
  records <-
    liftIO $
      Sqlite.executeSql
        db
        [ "SELECT hash, data, type FROM node",
          "WHERE data LIKE ? LIMIT ?;"
        ]
        [SQLText pattern, SQLInteger limit]
  pure . QueryResult (fromIntegral total) $
    Map.fromList $
      records <&> \[SQLText hash, SQLText nodeData, SQLText nodeType] ->
        (hash, Node nodeData nodeType)
