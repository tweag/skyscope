{-# OPTIONS_GHC -fwarn-unused-imports #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Category ((>>>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (flushTQueue, newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (modifyTVar, newTVarIO, readTVar, readTVarIO, stateTVar)
import Control.Monad (join, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, evalState, execState, gets, modify)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (FromJSON, FromJSONKey, FromJSONKeyFunction(..), ToJSON, ToJSONKey)
import qualified Data.Aeson as Json
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Attoparsec.Combinator as Parser
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (bimap)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Coerce (coerce)
import Data.FileEmbed (embedFile, embedFileIfExists)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (nub, uncons)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence ((><), Seq, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import Data.Traversable (for)
import Database.SQLite3 (SQLData(..))
import GHC.Generics (Generic)
import qualified Language.Haskell.TH as TH
import Network.HTTP.Types.Status (badRequest400)
import Prelude
import qualified Sqlite
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath.Find ((~~?), filePath, find)
import System.Posix.Temp (mkdtemp)
import System.Process.Text (readProcessWithExitCode)
import qualified Web.Scotty as Web

main :: IO ()
main = do
  dir <- mkdtemp "skyscope"
  let path = dir <> "/database"
  absolutePath <- getCurrentDirectory <&> (<> ("/" <> path))
  putStrLn $ "\x1b[1;33mdatabase: " <> absolutePath <> "\x1b[0m"
  Sqlite.withDatabase path $ \db -> importGraph db *> server db

data Node = Node
  { nodeType :: Text
  , nodeData :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype NodeHash = NodeHash Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FromJSONKey NodeHash where
  fromJSONKey = FromJSONKeyCoerce

instance ToJSONKey NodeHash where
  toJSONKey = toJSONKeyText coerce

data NodeState
  = Collapsed
  | Expanded
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Edge = Edge
  { edgeSource :: NodeHash
  , edgeTarget :: NodeHash
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type Graph = (Map NodeHash Node, Set Edge)

importGraph :: Sqlite.Database -> IO ()
importGraph db = do
  createSchema db
  (nodes, edges) <- Text.getContents <&> Parser.parseOnly graphParser >>= \case
    Left err -> error $ "failed to parse skyframe graph: " <> err
    Right graph -> pure graph
  let assignIndex node = modify (+ 1) *> gets (, node)
      indexedNodes = evalState (for nodes assignIndex) 0
      coerceMaybe = fromMaybe $ error "parser produced an edge with an unknown node"
      indexedEdges = coerceMaybe $ for (Set.toList edges) $ \(Edge source target) -> do
        (sourceIndex, _) <- Map.lookup source indexedNodes
        (targetIndex, _) <- Map.lookup target indexedNodes
        pure (sourceIndex, targetIndex)
  Sqlite.batchInsert db "node" [ "id", "hash", "type", "data" ] $
    Map.assocs indexedNodes <&> \(NodeHash hash, (id, Node nodeType nodeData)) ->
      SQLInteger id : (SQLText <$> [hash, nodeType, nodeType <> ":" <> nodeData ])
  Sqlite.batchInsert db "edge" [ "source", "target" ] $
    indexedEdges <&> \(s, t) -> SQLInteger <$> [ s, t ]
  indexPaths db
  where
    createSchema db = Sqlite.executeStatements db
      [ [ "CREATE TABLE node ("
        , "  id INTEGER,"
        , "  hash TEXT,"
        , "  type TEXT,"
        , "  data TEXT,"
        , "  PRIMARY KEY (id),"
        , "  UNIQUE (hash)"
        , ");"
        ]
      , [ "CREATE TABLE edge ("
        , "  source INTEGER,"
        , "  target INTEGER,"
        , "  PRIMARY KEY (source, target),"
        , "  FOREIGN KEY (source) REFERENCES node(id),"
        , "  FOREIGN KEY (target) REFERENCES node(id)"
        , ");"
        ]
      , [ "CREATE TABLE path ("
        , "  destination INTEGER,"
        , "  origin INTEGER,"
        , "  next INTEGER,"
        , "  PRIMARY KEY (destination, origin),"
        , "  FOREIGN KEY (destination) REFERENCES node(id),"
        , "  FOREIGN KEY (origin) REFERENCES node(id),"
        , "  FOREIGN KEY (next) REFERENCES node(id)"
        , ");"
        ]
      ]

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
      pure (Map.fromList nodes, Set.fromList $ Edge sourceHash . fst <$> targets)
    nodeParser :: Parser (NodeHash, Node)
    nodeParser = do
      nodeType <- Parser.takeWhile1 $ \c -> c /= ':' && c /= '\n'
      Parser.char ':'
      nodeData <- Parser.takeTill $ \c -> c == '|' || c == '\n'
      Parser.option undefined $ Parser.char '|'
      let nodeHash = NodeHash $ Text.decodeUtf8 $ LBSC.toStrict $ toLazyByteString $
            byteStringHex $ SHA256.hash $ Text.encodeUtf8 $ nodeType <> ":" <> nodeData
      pure (nodeHash, Node nodeType nodeData)

server :: Sqlite.Database -> IO ()
server db = do
  putStrLn $ "\nOpen this link in your browser: \x1b[1;36mhttp://localhost:28581/\x1b[0m\n"
  Web.scotty 28581 $ do
    Web.get "/" $ do
      Web.html $ LazyText.fromStrict $ Text.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "  <head>"
        , "    <title>Skyscope</title>"
        , "    <meta charset=\"UTF-8\">"
        , "    <script>"
        ,       Text.decodeUtf8 $ fromMaybe "" $(embedFileIfExists $(do
                  found <- TH.runIO $ find (pure True) (filePath ~~? "**/frontend/index.js") "."
                  pure $ TH.LitE $ TH.StringL $ case found of
                    [ path ] -> path
                    [] -> ""
                ))
        , "    </script>"
        , "    <style>"
        ,       Text.decodeUtf8 $(embedFile "frontend/src/theme.css")
        , "    </style>"
        , "  </head>"
        , "  <body></body>"
        , "</html>"
        ]
    Web.post "/find" $ Json.eitherDecode <$> Web.body >>= \case
      Right pattern -> Web.json =<< liftIO (findNodes db 100 pattern)
      Left err -> badRequest err
    Web.post "/path" $ Json.eitherDecode <$> Web.body >>= \case
      Right (node1, node2) -> Web.json =<< liftIO (findPath db node1 node2)
      Left err -> badRequest err
    Web.post "/render" $ Json.eitherDecode <$> Web.body >>= \case
      Right visibleNodes -> do
        Web.setHeader "Content-Type" "image/svg+xml"
        Web.text =<< liftIO (renderSvg db visibleNodes)
      Left err -> badRequest err
  where
    badRequest = Web.raiseStatus badRequest400 . LazyText.pack
    favicon = "<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 "
      <> "100 100%22><text y=%22.9em%22 font-size=%2290%22>🔭</text></svg>"

findNodes :: Sqlite.Database -> Int64 -> Text -> IO (Int64, Map NodeHash Node)
findNodes db limit pattern = do
  SQLInteger total <- Sqlite.executeSqlScalar db
    [ "SELECT COUNT(hash) FROM node"
    , "WHERE data LIKE ?" ] [ SQLText pattern ]
  records <- Sqlite.executeSql db
    [ "SELECT hash, type, data FROM node"
    , "WHERE data LIKE ? LIMIT ?;" ]
    [ SQLText pattern, SQLInteger limit ]
  pure . (total, ) $ Map.fromList $ records <&> \
    [ SQLText hash, SQLText nodeType, SQLText nodeData ] ->
      (NodeHash hash, Node nodeType nodeData)

findPath :: Sqlite.Database -> NodeHash -> NodeHash -> IO [NodeHash]
findPath db node1 node2 = pure [ node1, NodeHash "111", NodeHash "222", node2 ]

indexPaths :: Sqlite.Database -> IO ()
indexPaths db = do
  predecessors <- IntMap.fromAscListWith (++)
    . (map $ \[ SQLInteger t, SQLInteger s ] -> (fromIntegral t, [ fromIntegral s ]))
    <$> Sqlite.executeSql db [ "SELECT target, source FROM edge ORDER BY target;" ] []

  let incomingEdges :: Int -> Seq (Int, Int)
      incomingEdges node = Seq.fromList $ map (node, ) $ fromMaybe [] $ IntMap.lookup node predecessors

      step :: Seq (Int, Int) -> State (IntMap Int) (Seq Int)
      step frontier = case Seq.viewl frontier of
        (current, next) :< frontier -> do
          visited <- gets $ isJust . IntMap.lookup next
          if visited then step frontier else do
            modify $ IntMap.insert next current
            step $ frontier >< incomingEdges next
        EmptyL -> pure Seq.empty

  Sqlite.executeSql db [ "DELETE FROM path;" ] []
  indices <- newTVarIO =<< (map $ \[ SQLInteger id ] -> fromIntegral id) <$>
    Sqlite.executeSql db [ "SELECT id FROM node ORDER BY hash;" ] []

  let nextIndex = atomically $ stateTVar indices $ uncons >>> \case
        Just (next, remaining) -> (Just next, remaining)
        Nothing -> (Nothing, [])
      worker = nextIndex >>= \case
        Just destination -> do
          let frontier = incomingEdges destination
              paths = execState (step frontier) IntMap.empty
          Sqlite.batchInsert db "path" [ "destination", "origin", "next" ] $ IntMap.assocs paths
              <&> \(origin, next) -> SQLInteger . fromIntegral <$> [ destination, origin, next ]
          worker
        Nothing -> pure ()

  forkIO worker
  forkIO worker
  forkIO worker
  forkIO worker
  forkIO worker
  forkIO worker

  let waitLoop = length <$> readTVarIO indices >>= \len ->
        if len == 0 then pure () else do
          putStrLn $ show len <> " remaining"
          threadDelay 5000000
          waitLoop

  waitLoop

{-
  queue <- newTQueueIO
  running <- newTVarIO $ length indices
-}

{-
  remaining <- newTVarIO $ length indices

  for_ (take 1 indices) $ \destination -> forkIO $ do
    let frontier = incomingEdges destination
        paths = execState (step frontier) Map.empty
    putStrLn $ "destination " <> show destination <> ": found paths from " <> show (Map.size paths) <> " nodes"
    Sqlite.batchInsert db "path" [ "destination", "origin", "next" ] $
      Map.assocs paths <&> \(origin, next) -> SQLInteger <$> [ destination, origin, next ]
    atomically $ modifyTVar remaining pred
    remaining <- atomically $ readTVar remaining
    putStrLn $ "remaining: " <> show remaining
-}

{-
    in atomically $ do
        traverse_ (writeTQueue queue) $ Map.assocs paths
          <&> \(origin, next) -> SQLInteger <$> [ destination, origin, next ]
        modifyTVar running pred
-}
    
{-
  let insertLoop = do
        (batch, running) <- atomically $ (,) <$> flushTQueue queue <*> readTVar running
        Sqlite.batchInsert db "path" [ "destination", "origin", "next" ] batch
        when (running > 0) insertLoop
  insertLoop
-}

renderSvg :: Sqlite.Database -> Map NodeHash NodeState -> IO LazyText.Text
renderSvg db nodeStates = do
  edges <- fmap (nub . concat) $ flip Map.traverseWithKey nodeStates $
    \(NodeHash hash) state -> Sqlite.executeSql db
      [ "SELECT s.hash, t.hash FROM edge"
      , "INNER JOIN node AS s ON edge.source = s.id"
      , "INNER JOIN node AS t ON edge.target = t.id"
      , "WHERE s.hash = ?1 OR t.hash = ?1;" ] [ SQLText hash ]
      <&> (>>= \[ SQLText s, SQLText t ] ->
                    let source = NodeHash s
                        target = NodeHash t
                        collapsed = state == Collapsed
                        hidden  = (`Map.notMember` nodeStates)
                    in if collapsed && (hidden source || hidden target)
                        then [] else [ Edge (NodeHash s) (NodeHash t) ]
          )
  let nodeIdentityMap :: Map NodeHash NodeHash
      nodeIdentityMap = Map.fromList $ map (join (,)) $ Map.keys nodeStates <>
        (edges >>= \(Edge source target) -> [ source, target ])
  nodeMap <- for nodeIdentityMap $ \(NodeHash hash) -> Sqlite.executeSql db
    [ "SELECT type, data FROM node WHERE hash = ?;" ] [ SQLText hash ] <&> \
    [ [ SQLText nodeType, SQLText nodeData ] ] -> Node nodeType nodeData
  let graph = Text.unlines
        [ "digraph {"
        , "    pad=10"
        , "    node" <> graphvizAttributes
                [ ("color", "#efefef")
                , ("penwidth", "0.2")
                , ("style", "filled,rounded")
                ]
        , "    edge" <> graphvizAttributes
                [ ("arrowsize", "0.5")
                , ("color", "#3f3f3f")
                , ("penwidth", "0.2")
                ]
        , Text.unlines $ uncurry graphvizNode <$> Map.assocs nodeMap
        , Text.unlines $ graphvizEdge <$> edges
        , "}"
        ]
  Text.writeFile "/tmp/skyscope.dot" graph  -- For debugging
  readProcessWithExitCode "dot" [ "-Tsvg" ] graph <&> \case
    (ExitFailure code, _, err) -> error $ "dot exit " <> show code <> ": " <> Text.unpack err
    (ExitSuccess, svg, _) -> LazyText.fromStrict svg

  where
    graphvizNode :: NodeHash -> Node -> Text
    graphvizNode nodeHash@(NodeHash hash) (Node nodeType nodeData) =
      let truncatedNodeData = Text.take 8192 nodeData
          label = nodeType <> "\\n" <> truncatedNodeData
          nodeState = Map.lookup nodeHash nodeStates
          hidden = nodeState == Nothing
      in "    node_" <> hash <> graphvizAttributes
              [ ("width", if hidden then "0.1" else "3.0")
              , ("height", if hidden then "0.1" else "0.6")
              , ("shape", if hidden then "point" else "box")
              , ("fixedsize", "true")
              , ("label", label)
              , ("id", hash)
              , ("class", Text.pack $ fromMaybe "" $ show <$> nodeState)
              , ("tooltip", truncatedNodeData <> "\n\n" <> case nodeState of
                  Just Expanded -> "Click to collapse this node and hide its edges. Hold CTRL and click to hide it entirely."
                  Just Collapsed -> "Click to expand this node and show its edges. Hold CTRL and click to hide it."
                  Nothing -> "Click to show this node."
                )
              ]

    graphvizEdge :: Edge -> Text
    graphvizEdge (Edge (NodeHash source) (NodeHash target)) =
      "    node_" <> source <> " -> node_" <> target <>
      graphvizAttributes [ ("id", source <> "_" <> target) ]

    graphvizAttributes :: [(Text, Text)] -> Text
    graphvizAttributes attrs =
      let f (name, value) = name <> "=\"" <> value <> "\""
      in " [ " <> Text.intercalate "; " (f <$> attrs) <> " ];"




--  Sqlite.batchInsert db "node" [ "id", "hash", "type", "data" ] $
--    Map.assocs indexedNodes <&> \(NodeHash hash, (id, Node nodeType nodeData)) ->
--      SQLInteger id : (SQLText <$> [hash, nodeType, nodeType <> ":" <> nodeData ])
  

  --let paths = execState (step $ incomingEdges 1397) Map.empty  -- StarlarkBuiltins
  --let paths = execState (step $ incomingEdges 653) Map.empty
--  let paths = execState (step $ incomingEdges 7) Map.empty
--  putStrLn $ "length paths = " <> show (length paths) <> "\n "
--  for (Map.assocs paths) $ \(x, y) -> putStrLn $ "from " <> show x <> " goto " <> show y
--
--  pure ()

-- -- $> withDatabase "/home/ben/.cache/bazel/_bazel_ben/ba67dc5f229eae66a85329faeb16e66d/execroot/skyscope/bazel-out/k8-fastbuild/bin/backend/skyscope.runfiles/skyscope/skyscope8TI9BC/database" Main.indexPaths
