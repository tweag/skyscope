{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue)
import Control.Concurrent.STM.TQueue (newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Exception (tryJust)
import Control.Monad (guard, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Attoparsec.Combinator as Parser
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Char (isDigit, isUpper)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.List (nub, unzip)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Database.SQLite3 (SQLData(..))
import qualified Database.SQLite3 as SQL
import Prelude
import qualified Sqlite
import System.IO (IOMode(..), withFile)
import System.IO.Error (isEOFError)
import System.Posix.Temp (mkdtemp)

data Node = Node
  { nodeType :: Text
  , nodeData :: Text
  } deriving (Eq, Ord, Show)

newtype NodeHash = NodeHash Text
  deriving (Eq, Ord, Show)

newtype Edge = Edge (NodeHash, NodeHash)
  deriving (Eq, Ord, Show)

edgeSource :: Edge -> NodeHash
edgeSource (Edge (source, _)) = source

edgeTarget :: Edge -> NodeHash
edgeTarget (Edge (_, target)) = target

type Graph = (Map NodeHash Node, Set Edge)

graphParser :: Parser Graph
graphParser = do
  Parser.option undefined $ Parser.string "Warning:"
  Parser.manyTill Parser.anyChar $ Parser.lookAhead lineParser
  let unions = bimap Map.unions Set.unions . unzip
  graph <- unions <$> lineParser `Parser.sepBy` Parser.endOfLine
  Parser.many' Parser.endOfLine
  Parser.endOfInput
  pure graph
  where
    lineParser :: Parser Graph
    lineParser = do
      nodes@((sourceHash, _) : targets) <- Parser.many1 nodeParser
      pure (Map.fromList nodes, Set.fromList $ Edge . (sourceHash, ) . fst <$> targets)
    nodeParser :: Parser (NodeHash, Node)
    nodeParser = do
      nodeType <- Parser.takeWhile1 $ \c -> c /= ':' && c /= '\n'
      Parser.char ':'
      nodeData <- Parser.takeTill $ \c -> c == '|' || c == '\n'
      Parser.option undefined $ Parser.char '|'
      let nodeHash = NodeHash $ Text.decodeUtf8 $ LBSC.toStrict $ toLazyByteString $
            byteStringHex $ SHA256.hash $ Text.encodeUtf8 $ nodeType <> ":" <> nodeData
      pure (nodeHash, Node{..})



-- $> fmap (Data.Bifunctor.bimap (map Main.nodeType . Data.Map.elems) Data.Set.toList) $ Data.Attoparsec.Text.parseOnly Main.graphParser $ Text.pack "Warning:\n\nGLOB:<GlobDescriptor packageName=@stackage// packageRoot=/home/ben/.cache/bazel/_bazel_ben/b9e44cbefb573eab6fcf0cf1e5b6269e/external/stackage subdir=base-compat-0.11.2/src/Data/Bitraversable pattern=** globberOperation=FILES>|IGNORED_PACKAGE_PREFIXES:@stackage|PACKAGE_LOOKUP:@stackage//base-compat-0.11.2/src/Data/Bitraversable|DIRECTORY_LISTING:[/home/ben/.cache/bazel/_bazel_ben/b9e44cbefb573eab6fcf0cf1e5b6269e/external/stackage]/[base-compat-0.11.2/src/Data/Bitraversable]|GLOB:<GlobDescriptor packageName=@stackage// packageRoot=/home/ben/.cache/bazel/_bazel_ben/b9e44cbefb573eab6fcf0cf1e5b6269e/external/stackage subdir=base-compat-0.11.2/src/Data/Bitraversable/Compat pattern=** globberOperation=FILES>\nFILE:[/home/ben/.cache/bazel/_bazel_ben/b9e44cbefb573eab6fcf0cf1e5b6269e/external/com_google_protobuf]/[csharp/src/Google.Protobuf/WellKnownTypes/BUILD]|FILE:[/home/ben/.cache/bazel/_bazel_ben/b9e44cbefb573eab6fcf0cf1e5b6269e/external/com_google_protobuf]/[csharp/src/Google.Protobuf/WellKnownTypes]|FILE_STATE:[/home/ben/.cache/bazel/_bazel_ben/b9e44cbefb573eab6fcf0cf1e5b6269e/external/com_google_protobuf]/[csharp/src/Google.Protobuf/WellKnownTypes/BUILD]\n\n"

main :: IO ()
main = do
  putStrLn "\x1b[1;36mBEGIN\x1b[0m"
  (nodes, edges) <- Parser.parseOnly graphParser <$> Text.getContents >>= \case
    Left err -> error $ "failed to parse skyframe graph: " <> err
    Right graph -> pure graph
  putStrLn $ "node count = " <> show (Map.size nodes) <> ", edge count = " <> show (Set.size edges)
  putStrLn $ "unique sources = " <> show (Set.size $ Set.map edgeSource $ edges)
  putStrLn $ "unique targets = " <> show (Set.size $ Set.map edgeTarget $ edges)
  withFile "/tmp/skylight.dot" WriteMode $ \handle -> do
    let writeLine = Text.hPutStrLn handle
        contains str nodeData = not $ null $ Text.breakOnAll str nodeData
    writeLine "digraph {"
    for_ (Map.assocs nodes) $ \(NodeHash hash, Node{..}) ->
      when (contains "cpp:toolchain" nodeData) $ writeLine $ "    node_" <> hash <> " [label=\"" <> nodeType <> "\",tooltip=\"" <> Text.take 256 nodeData <> "\"];"
    for_ (Set.toList edges) $ \(Edge (NodeHash source, NodeHash target)) ->
      when (all (contains "cpp:toolchain" . nodeData . fromJust . flip Map.lookup nodes) [ NodeHash source, NodeHash target ]) $ writeLine $ "    node_" <> source <> " -> node_" <> target <> ";"
    writeLine "}"
  putStrLn "\x1b[1;36mEND\x1b[0m"

processLines :: TQueue (NonEmpty (Text, Text)) -> TVar Bool -> IO ()
processLines queue done = getLine >>= \case
  Right line -> case nonEmpty (Text.splitOn "|" line) of
    Just keys -> do
      let pairs = keys <&> \key -> case Text.span isNameChar key of
            (name, remaining) -> case Text.span (== ':') remaining of
              (":", body) -> undefined
      atomically $ writeTQueue queue pairs
      loop
    Nothing -> loop
  Left _ -> atomically $ writeTVar done True
  where
    getLine = tryJust (guard . isEOFError) Text.getLine
    isNameChar c = isUpper c || isDigit c || c == '_'
    loop = processLines queue done

importGraph :: IO FilePath
importGraph = do
  undefined
{-
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
          rawText -> do
            let nodes = nub $ concat $ NonEmpty.toList <$> rawText
            Sqlite.batchInsert db "node" [ "hash", "type", "data" ] "ON CONFLICT DO NOTHING" $
              nodes <&> \(node) -> [ SQLText (hashKey node), SQLText node ]
            putStrLn $ "inserted " <> show (length nodes) <> " nodes"
            let edges = concat $ rawText <&> \(source :| targets) -> (hashKey source, ) . hashKey <$> targets
            Sqlite.batchInsert db "edge" [ "source", "target" ] "" $
              edges <&> \(source, target) -> [ SQLText source, SQLText target ]
            putStrLn $ "inserted " <> show (length edges) <> " edges"
            insertLoop
    insertLoop
  pure path
-}

createSchema :: Sqlite.Database -> IO ()
createSchema database = Sqlite.executeStatements database
  [ [ "CREATE TABLE node ("
    , "  hash TEXT PRIMARY KEY,"
    , "  type TEXT,"
    , "  body TEXT"
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
