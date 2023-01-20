{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue)
import Control.Concurrent.STM.TQueue (newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Exception (tryJust)
import Control.Monad (guard)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Database.SQLite3 (SQLData(..))
import qualified Database.SQLite3 as SQL
import Prelude
import qualified Sqlite
import System.IO.Error (isEOFError)
import System.Posix.Temp (mkdtemp)

main :: IO ()
main = do
  putStrLn "\x1b[1;36mBEGIN\x1b[0m"
  path <- importGraph
  putStrLn path
  putStrLn "\x1b[1;36mEND\x1b[0m"

processLines :: TQueue (NonEmpty Text) -> TVar Bool -> IO ()
processLines queue done = getLine >>= \case
  Right line -> case nonEmpty (Text.splitOn "|" line) of
    Just keys -> atomically (writeTQueue queue keys) *> loop
    Nothing -> loop
  Left _ -> atomically $ writeTVar done True
  where
    getLine = tryJust (guard . isEOFError) Text.getLine
    loop = processLines queue done

importGraph :: IO FilePath
importGraph = do
  queue <- newTQueueIO
  done <- newTVarIO False
  forkIO $ processLines queue done
  dir <- mkdtemp "skylight"
  let path = dir <> "/database"
  Sqlite.withDatabase path $ \db _ -> do
    createSchema db
    let insertLoop = atomically (flushTQueue queue) >>= \case
          [] -> readTVarIO done >>= \case
            True -> pure ()
            False -> insertLoop
          batch -> do
            let nodes = nub $ concat $ NonEmpty.toList <$> batch
            Sqlite.batchInsert db "node" [ "hash", "data" ] "ON CONFLICT DO NOTHING" $
              nodes <&> \node -> [ SQLText (hashKey node), SQLText node ]
            putStrLn $ "inserted " <> show (length nodes) <> " nodes"
            let edges = concat $ batch <&> \(source :| targets) -> (hashKey source, ) . hashKey <$> targets
            Sqlite.batchInsert db "edge" [ "source", "target" ] "" $
              edges <&> \(source, target) -> [ SQLText source, SQLText target ]
            putStrLn $ "inserted " <> show (length edges) <> " edges"
            insertLoop
    insertLoop
  pure path

createSchema :: Sqlite.Database -> IO ()
createSchema database = Sqlite.executeStatements database
  [ [ "CREATE TABLE node ("
    , "  hash TEXT PRIMARY KEY,"
    , "  data TEXT"
    , ");"
    ]
  , [ "CREATE TABLE edge ("
    , "  source TEXT,"
    , "  target TEXT,"
    , "  FOREIGN KEY (source) REFERENCES node(hash),"
    , "  FOREIGN KEY (target) REFERENCES node(hash)"
    , ");"
    ]
  ]

hashKey :: Text -> Text
hashKey = Text.decodeUtf8 . LBSC.toStrict . toLazyByteString . byteStringHex . SHA256.hash . Text.encodeUtf8
