{-# OPTIONS_GHC  #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Category ((>>>))
import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVarIO, stateTVar, writeTVar)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.RWS.Class
import Control.Monad.State (evalState)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (FromJSON, FromJSONKey, FromJSONKeyFunction(..), ToJSON, ToJSONKey)
import qualified Data.Aeson as Json
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Attoparsec.Combinator as Parser
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Coerce (coerce)
import Data.FileEmbed (embedFile, embedFileIfExists)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (nub, uncons)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Time.Clock as Clock
import Data.Traversable (for)
import Database.SQLite3 (SQLData(..))
import Foreign.C.Types (CInt(..), CLong(..))
import qualified Foreign.Marshal.Array as Marshal
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf)
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
  Sqlite.withDatabase path $ \db -> do
    Sqlite.executeStatements db
      [ [ "pragma journal_mode = WAL;" ]
      , [ "pragma mmap_size = 1073741824;" ]
      , [ "pragma synchronous = off;" ]
      ]
    importGraph db
    progress <- newTVarIO (0, 1)
    indexStartTime <- Clock.getCurrentTime
    forkIO $ indexPaths db progress
    let indexPathProgress = do
          (done, total) <- readTVarIO progress
          if done == total then pure () else do
            timeTaken <- Clock.getCurrentTime <&> (`Clock.diffUTCTime` indexStartTime)
            let unitTime = Clock.nominalDiffTimeToSeconds timeTaken / fromInteger (fromIntegral $ max 1 done)
                expectedRemaining = ceiling $ fromInteger (fromIntegral $ total - done) * unitTime
            putStrLn $ "\x1b[1F\x1b[2Kindexing paths to " <> show done <> " / " <> show total
              <> " nodes (" <> show expectedRemaining <> " seconds remaining)"
            threadDelay 100000
            indexPathProgress
    forkIO indexPathProgress
    server db

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
importGraph db = timed "importGraph" $ do
  createSchema db
  (nodes, edges) <- Text.getContents <&> Parser.parseOnly graphParser >>= \case
    Left err -> error $ "failed to parse skyframe graph: " <> err
    Right graph -> pure graph
  let assignIndex node = gets (, node) <* modify (+ 1)
      indexedNodes = evalState (for nodes assignIndex) 1
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
        , "  steps BLOB,"
        , "  PRIMARY KEY (destination)"
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
      Right (origin, destination) -> Web.json =<< liftIO (findPath db origin destination)
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
findPath db (NodeHash origin) (NodeHash destination) = do
  let getNodeId hash = Sqlite.executeSqlScalar db
        [ "SELECT id FROM node WHERE hash = ?;" ]
        [ SQLText hash ] <&> fromSQLInt
  origin <- getNodeId origin
  destination <- getNodeId destination
  steps <- Sqlite.executeSql db
    [ "SELECT steps FROM path"
    , "WHERE destination = ?;" ]
    [ SQLInteger destination ]
  case steps of
    [] -> error $ "no path data for " <> show destination
    [ [ SQLBlob blob ] ] -> do
      let bytes = BS.unpack blob
          stepMapSizeBytes = length bytes
          stepMapSize = stepMapSizeBytes `div` sizeOf (0 :: CLong)
      if stepMapSize * sizeOf (0 :: CLong) /= stepMapSizeBytes
          then error $ "malformed path data for " <> show destination
          else Marshal.allocaArray stepMapSize $ \stepMapPtr -> do
            Marshal.pokeArray (castPtr stepMapPtr) bytes
            let maxPathSize = 1024
            Marshal.allocaArray maxPathSize $ \pathPtr -> do
              pathSize <- fromIntegral <$> Main.c_findPath
                (fromIntegral origin) (fromIntegral destination)
                stepMapPtr (fromIntegral stepMapSize)
                pathPtr (fromIntegral maxPathSize)
              path <- Marshal.peekArray pathSize pathPtr
              for (fromIntegral <$> path) $ \node -> Sqlite.executeSql db
                [ "SELECT hash FROM node WHERE id = ?;" ] [ SQLInteger node ] <&> \case
                  [] -> error $ "failed to find hash for node " <> show node
                  [ [ SQLText hash ] ] -> NodeHash hash

foreign import ccall safe "path.h" c_findPath
  :: CInt -- origin
  -> CInt -- destination
  -> Ptr CLong -- stepMap
  -> CInt -- stepMapSize
  -> Ptr CInt -- buffer
  -> CInt -- size
  -> IO CInt

indexPaths :: Sqlite.Database -> TVar (Int, Int) -> IO ()
indexPaths db progress = timed "indexPaths" $ do
  Sqlite.executeSql db [ "DELETE FROM path;" ] []
  nodeCount <- Sqlite.executeSqlScalar db [ "SELECT COUNT(id) FROM node;" ] [] <&> fromSQLInt
  atomically $ writeTVar progress (0, nodeCount)
  predMap <- makePredMap nodeCount
  let predMapSize = length predMap
  Marshal.allocaArray predMapSize $ \predMapPtr -> do
    Marshal.pokeArray predMapPtr $ fromIntegral <$> predMap
    destinations <- newTVarIO [ 1 .. fromIntegral nodeCount ]
    let nextDest = atomically $ stateTVar destinations $ uncons >>> \case
          Just (next, remaining) -> (Just next, remaining)
          Nothing -> (Nothing, [])
        worker stepMapPtr = nextDest >>= \case
          Just destination -> do
            stepMapSize <- Main.c_indexPaths destination
              predMapPtr
              stepMapPtr (fromIntegral nodeCount)
            let stepMapSizeBytes = fromIntegral stepMapSize * sizeOf (0 :: CLong)
            stepMapBytes <- Marshal.peekArray stepMapSizeBytes $ castPtr stepMapPtr
            Sqlite.executeSql db [ "INSERT INTO path (destination, steps) VALUES (?, ?);" ]
              [ SQLInteger $ fromIntegral destination, SQLBlob $ BS.pack stepMapBytes ]
            atomically $ modifyTVar progress $ first (+ 1)
            worker stepMapPtr
          Nothing -> pure ()
    workerCount <- getNumCapabilities <&> (subtract 1)
    for_ [ 1 .. max 1 workerCount ] (const $ forkIO $ Marshal.allocaArray nodeCount worker)
    let wait = length <$> readTVarIO destinations >>= \len -> if len == 0 then pure () else wait
    wait

  where
    makePredMap :: Int -> IO [Int]
    makePredMap nodeCount = do
      -- TODO: Document the rationale behind this cache-friendly data structure.
      predecessors <- Sqlite.executeSql db [ "SELECT target, source FROM edge ORDER BY target;" ] []
        <&> ((map $ \[ t, s ] -> (fromSQLInt t, [ fromSQLInt s ])) >>> IntMap.fromAscListWith (++))
      pure $ uncurry (++) $ evalRWS (for [ 0 .. nodeCount ] serialisePreds) predecessors 0

    serialisePreds :: Int -> RWS (IntMap [Int]) [Int] Int Int
    serialisePreds i = asks (IntMap.lookup i) <&> fromMaybe [] >>= \case
      [ pred ] -> pure pred
      preds -> do
        let n = length preds
        tell $ n : preds
        offset <- get <* modify (+ (1 + n))
        pure $ negate offset

foreign import ccall safe "path.h" c_indexPaths
  :: CInt -- destination
  -> Ptr CInt -- predMap
  -> Ptr CLong -- stepMap
  -> CInt -- nodeCount
  -> IO CInt

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
  links <- findLinks db nodeMap
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
        , Text.unlines $ graphvizEdge [] <$> edges
        , Text.unlines $
            let tooltip n =  "Click here to show " <> Text.pack (show n) <> " hidden nodes."
            in links <&> \(edge, n) -> graphvizEdge
                [ ("arrowhead", "none")
                , ("class", "Path")
                , ("label", "Open path")
                , ("labeltooltip", tooltip n)
                , ("penwidth", "1.0")
                , ("style", "dotted")
                , ("tooltip", tooltip n)
                ] edge
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

    graphvizEdge :: [(Text, Text)] -> Edge -> Text
    graphvizEdge attrs (Edge (NodeHash source) (NodeHash target)) =
      "    node_" <> source <> " -> node_" <> target <>
      graphvizAttributes (attrs ++ [ ("id", source <> "_" <> target) ])

    graphvizAttributes :: [(Text, Text)] -> Text
    graphvizAttributes attrs =
      let f (name, value) = name <> "=\"" <> value <> "\""
      in " [ " <> Text.intercalate "; " (f <$> attrs) <> " ];"

findLinks :: Sqlite.Database -> Map NodeHash Node -> IO [(Edge, Int)]
findLinks db _ = pure
  [ (Edge
      (NodeHash "e2f4994c487d67f3f31ec811178ba4bbc6f2add3cf48a0c2982801f41658bebc")
      (NodeHash "f307777884f3e174a91e18f9ab3f4bfdd7c74d6cc20619e6ba0545b879a6bd76")
    , 19
    )
  ]

fromSQLInt :: Num a => SQLData -> a
fromSQLInt (SQLInteger n) = fromIntegral n

timed :: String -> IO a -> IO a
timed label action = do
  startTime <- Clock.getCurrentTime
  result <- action
  endTime <- Clock.getCurrentTime
  putStrLn $ "\n\n" <> label <> " took " <> show (Clock.nominalDiffTimeToSeconds $ Clock.diffUTCTime endTime startTime) <> " seconds\n"
  pure result
