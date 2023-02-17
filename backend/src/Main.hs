{-# OPTIONS_GHC  #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Monad (join, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.RWS.Class
import Control.Monad.State (State, evalState)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Json
import qualified Data.Attoparsec.Combinator as Parser
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.FileEmbed (embedFile, embedFileIfExists)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (nub, sortOn, tails, uncons)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
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
import Debug.Trace (trace)
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
import System.IO (hSetEncoding, stdout, utf8)
import System.Posix.Temp (mkdtemp)
import System.Process (readProcess)
import System.Process.Text (readProcessWithExitCode)
import qualified Web.Scotty as Web

main :: IO ()
main = do
  hSetEncoding stdout utf8
  dir <- mkdtemp "skyscope"
  let dbPath = dir <> "/skyscope.sqlite"
  absolutePath <- getCurrentDirectory <&> (<> ("/" <> dbPath))
  putStrLn $ "\x1b[1mimporting graph:\x1b[0;30m " <> absolutePath <> "\x1b[0m"
  Sqlite.withDatabase dbPath $ \db -> do
    Sqlite.executeStatements db
      [ [ "pragma journal_mode = WAL;" ]
      , [ "pragma mmap_size = 1073741824;" ]
      , [ "pragma synchronous = off;" ]
      ]
    importGraph db
    progress <- newTVarIO (0, 1)
    indexStartTime <- Clock.getCurrentTime
    forkIO $ indexPaths db progress
    let showProgress = do
          threadDelay 100_000
          (done, total) <- readTVarIO progress
          timeTaken <- Clock.getCurrentTime <&> (`Clock.diffUTCTime` indexStartTime)
          let unitTime = Clock.nominalDiffTimeToSeconds timeTaken / fromInteger (fromIntegral $ max 1 done)
              expectedTime = ceiling $ fromInteger (fromIntegral $ total - done) * unitTime
              ansi = if done < total then "37" else "32"
          putStrLn $ "\x1b[1F\x1b[2K\x1b[" <> ansi <> "mindexed paths to "
              <> show done <> " nodes (" <> show total <> " total, "
              <> show expectedTime <> " seconds remaining)\x1b[0m"
          if done < total then showProgress else do
              dbSize <- readProcess "bash" [ "-c", "sync; ls -sh " <> dbPath <> " | grep -Po '^\\w+'" ] ""
              putStrLn $ "  time taken = " <> show timeTaken <> " seconds"
              putStrLn $ "  database size = " <> dbSize
    forkIO showProgress
    server db

data Node = Node
  { nodeType :: Text
  , nodeData :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type NodeHash = Text

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
  Sqlite.batchInsert db "node" [ "idx", "hash", "type", "data" ] $
    Map.assocs indexedNodes <&> \(nodeHash, (nodeIdx, Node nodeType nodeData)) ->
      SQLInteger nodeIdx : (SQLText <$> [nodeHash, nodeType, nodeType <> ":" <> nodeData ])
  Sqlite.batchInsert db "edge" [ "source", "target" ] $
    indexedEdges <&> \(s, t) -> SQLInteger <$> [ s, t ]

  where
    createSchema db = Sqlite.executeStatements db
      [ [ "CREATE TABLE node ("
        , "  idx INTEGER,"
        , "  hash TEXT,"
        , "  type TEXT,"
        , "  data TEXT,"
        , "  PRIMARY KEY (idx),"
        , "  UNIQUE (hash)"
        , ");"
        ]
      , [ "CREATE TABLE edge ("
        , "  source INTEGER,"
        , "  target INTEGER,"
        , "  PRIMARY KEY (source, target),"
        , "  FOREIGN KEY (source) REFERENCES node(idx),"
        , "  FOREIGN KEY (target) REFERENCES node(idx)"
        , ");"
        ]
      , [ "CREATE TABLE path ("
        , "  destination INTEGER,"
        , "  steps BLOB,"
        , "  PRIMARY KEY (destination),"
        , "  FOREIGN KEY (destination) REFERENCES node(idx)"
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
      let nodeHash = Text.decodeUtf8 $ LBSC.toStrict $ toLazyByteString $
            byteStringHex $ SHA256.hash $ Text.encodeUtf8 $ nodeType <> ":" <> nodeData
      pure (nodeHash, Node nodeType nodeData)

server :: Sqlite.Database -> IO ()
server db = do
  putStrLn $ "\nOpen this link in your browser: 🔭 \x1b[1;36mhttp://localhost:28581/\x1b[0m\n"
  Web.scotty 28581 $ do
    Web.get "/" $ do
      Web.html $ LazyText.fromStrict $ Text.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "  <head>"
        , "    <title>Skyscope</title>"
        , "    <link rel=\"icon\" href=\"data:image/svg+xml," <> favicon <> "\">"
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
      (hash, Node nodeType nodeData)

findPath :: Sqlite.Database -> NodeHash -> NodeHash -> IO [NodeHash]
findPath db origin destination = selecting $ \origin destination steps -> do
  let stepMapBytes = BS.unpack steps
      stepMapSizeBytes = length stepMapBytes
      stepMapSize = stepMapSizeBytes `div` sizeOf (0 :: CLong)
  if stepMapSize * sizeOf (0 :: CLong) /= stepMapSizeBytes
      then error $ "misaligned path data for " <> show destination
      else Marshal.allocaArray stepMapSize $ \stepMapPtr -> do
        Marshal.pokeArray (castPtr stepMapPtr) stepMapBytes
        let maxLength = 1048576 -- 4MiB of int32_t
        Marshal.allocaArray maxLength $ \pathPtr -> do
          actualLength <- fromIntegral <$> Main.c_findPath
            (fromIntegral origin) (fromIntegral destination)
            stepMapPtr (fromIntegral stepMapSize)
            pathPtr (fromIntegral maxLength)
          if actualLength == -1 then error "exceeded max path length" else do
            path <- Marshal.peekArray actualLength pathPtr
            for (fromIntegral <$> path) $ \nodeIdx -> Sqlite.executeSql db
              [ "SELECT hash FROM node WHERE idx = ?;" ] [ SQLInteger nodeIdx ] <&> \case
                [] -> error $ "failed to find hash for path node " <> show nodeIdx
                [ [ SQLText nodeHash ] ] -> nodeHash

  where
    selecting :: (Int64 -> Int64 -> ByteString -> IO a) -> IO a
    selecting action = do
      origin <- getNodeIdx origin
      destination <- getNodeIdx destination
      steps <- Sqlite.executeSql db
        [ "SELECT steps FROM path"
        , "WHERE destination = ?;" ]
        [ SQLInteger destination ]
      case steps of
        [] -> error $ "no path data for " <> show destination
        [ [ SQLBlob steps ] ] -> action origin destination steps

    getNodeIdx :: Text -> IO Int64
    getNodeIdx hash = Sqlite.executeSqlScalar db
      [ "SELECT idx FROM node WHERE hash = ?;" ]
      [ SQLText hash ] <&> \(SQLInteger n) -> n

foreign import ccall safe "path.cpp" c_findPath
  :: CInt -- origin
  -> CInt -- destination
  -> Ptr CLong -- stepMap
  -> CInt -- stepMapSize
  -> Ptr CInt -- pathBuffer
  -> CInt -- maxLength
  -> IO CInt

indexPaths :: Sqlite.Database -> TVar (Int, Int) -> IO ()
indexPaths db progress = do
  Sqlite.executeSql db [ "DELETE FROM path;" ] []
  nodeCount <- Sqlite.executeSqlScalar db [ "SELECT COUNT(idx) FROM node;" ] [] <&> fromSQLInt
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
            stepMapSize <- Main.c_indexPaths predMapPtr destination (fromIntegral nodeCount) stepMapPtr
            let stepMapSizeBytes = fromIntegral stepMapSize * sizeOf (0 :: CLong)
            stepMapBytes <- Marshal.peekArray stepMapSizeBytes $ castPtr stepMapPtr
            Sqlite.executeSql db [ "INSERT INTO path (destination, steps) VALUES (?, ?);" ]
              [ SQLInteger $ fromIntegral destination, SQLBlob $ BS.pack stepMapBytes ]
            atomically $ modifyTVar progress $ first (+ 1)
            worker stepMapPtr
          Nothing -> pure ()
    workerCount <- getNumCapabilities <&> (subtract 1)
    for_ [ 1 .. max 1 workerCount ] $ const $ forkIO $ Marshal.allocaArray nodeCount worker
    let wait = atomically $ do
          remaining <- length <$> readTVar destinations
          if remaining > 0 then retry else pure ()
    wait

  where
    makePredMap :: Int -> IO [Int]
    makePredMap nodeCount = do
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

foreign import ccall safe "path.cpp" c_indexPaths
  :: Ptr CInt -- predMap
  -> CInt -- destination
  -> CInt -- nodeCount
  -> Ptr CLong -- stepMap
  -> IO CInt

renderSvg :: Sqlite.Database -> Map NodeHash NodeState -> IO LazyText.Text
renderSvg db nodeStates = do
  edges <- fmap (nub . concat) $ flip Map.traverseWithKey nodeStates $
    \nodeHash state -> Sqlite.executeSql db
      [ "SELECT s.hash, t.hash FROM edge"
      , "INNER JOIN node AS s ON s.idx = edge.source"
      , "INNER JOIN node AS t ON t.idx = edge.target"
      , "WHERE s.hash = ?1 OR t.hash = ?1;" ] [ SQLText nodeHash ]
      <&> (>>= \[ SQLText source, SQLText target ] ->
                    let collapsed = state == Collapsed
                        hidden  = (`Map.notMember` nodeStates)
                    in if collapsed && (hidden source || hidden target)
                        then [] else [ Edge source target ]
          )
  let nodeIdentityMap :: Map NodeHash NodeHash
      nodeIdentityMap = Map.fromList $ map (join (,)) $ Map.keys nodeStates <>
        (edges >>= \(Edge source target) -> [ source, target ])
  nodeMap <- for nodeIdentityMap $ \nodeHash -> Sqlite.executeSql db
    [ "SELECT type, data FROM node WHERE hash = ?;" ] [ SQLText nodeHash ] <&> \
    [ [ SQLText nodeType, SQLText nodeData ] ] -> Node nodeType nodeData
  links <- findPathLinks db nodeStates edges

  let graph = Text.unlines
        [ "digraph {"
        , "    pad=2"
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
            let tooltip hiddenCount = "Click here to show " <> Text.pack (show hiddenCount) <> " hidden nodes."
            in links <&> \(hiddenCount, edge) -> graphvizEdge
                [ ("arrowhead", "none")
                , ("class", "Path")
                , ("label", "Open path")
                , ("labeltooltip", tooltip hiddenCount)
                , ("penwidth", "1.0")
                , ("style", "dotted")
                , ("tooltip", tooltip hiddenCount)
                ] edge
        , "}"
        ]
  Text.writeFile "/tmp/skyscope.dot" graph  -- For debugging
  readProcessWithExitCode "dot" [ "-Tsvg" ] graph <&> \case
    (ExitFailure code, _, err) -> error $ "dot exit " <> show code <> ": " <> Text.unpack err
    (ExitSuccess, svg, _) -> LazyText.fromStrict svg

  where
    graphvizNode :: NodeHash -> Node -> Text
    graphvizNode nodeHash (Node nodeType nodeData) =
      let truncatedNodeData = Text.take 8192 nodeData
          label = nodeType <> "\\n" <> truncatedNodeData
          nodeState = Map.lookup nodeHash nodeStates
          hidden = nodeState == Nothing
      in "    node_" <> nodeHash <> graphvizAttributes
              [ ("width", if hidden then "0.1" else "3.0")
              , ("height", if hidden then "0.1" else "0.6")
              , ("shape", if hidden then "point" else "box")
              , ("fixedsize", "true")
              , ("label", label)
              , ("id", nodeHash)
              , ("class", Text.pack $ fromMaybe "" $ show <$> nodeState)
              , ("tooltip", truncatedNodeData <> "\n\n" <> case nodeState of
                  Just Expanded -> "Click to collapse this node and hide its edges. Hold CTRL and click to hide it entirely."
                  Just Collapsed -> "Click to expand this node and show its edges. Hold CTRL and click to hide it."
                  Nothing -> "Click to show this node."
                )
              ]

    graphvizEdge :: [(Text, Text)] -> Edge -> Text
    graphvizEdge attrs (Edge source target) =
      "    node_" <> source <> " -> node_" <> target <>
      graphvizAttributes (attrs ++ [ ("id", source <> "_" <> target) ])

    graphvizAttributes :: [(Text, Text)] -> Text
    graphvizAttributes attrs =
      let f (name, value) = name <> "=\"" <> value <> "\""
      in " [ " <> Text.intercalate "; " (f <$> attrs) <> " ];"

type Component = Set NodeHash

findPathLinks :: Sqlite.Database -> Map NodeHash NodeState -> [Edge] -> IO [(Int, Edge)]
findPathLinks db nodeStates edges = timed "findPathLinks" $ nub <$> do
  let components = findComponents edges $ Map.keysSet nodeStates
      pairs = concat [ [ (c1, c2), (c2, c1) ] | c1 : cs <- tails components, c2 <- cs ] -- m² pairs
  fmap catMaybes $ for pairs $ uncurry $ unifyComponents db nodeStates

findComponents :: [Edge] -> Set NodeHash -> [Component]
findComponents edges visibleNodes =
  let neighbourMap = flip Map.union (Map.fromSet (const []) visibleNodes) $ flip Map.restrictKeys visibleNodes
        $ Map.fromAscListWith (++) $ sortOn fst $ edges >>= \(Edge s t) -> [ (s, [ t ]), (t, [ s ]) ]

      findComponent :: State (Set NodeHash) (Maybe Component)
      findComponent = do
        unvisitedNodes <- gets $ Set.difference visibleNodes
        case Set.lookupMin unvisitedNodes of
          Nothing -> pure Nothing
          Just node -> do
            nodes <- Set.fromList <$> dfs [ [ node ] ]
                <&> (`Set.intersection` visibleNodes)
            pure $ Just nodes

      dfs :: [[NodeHash]] -> State (Set NodeHash) [NodeHash]
      dfs = \case
        [] -> pure []
        [] : stack -> dfs stack
        (next : siblings) : stack -> do
          let stack' = siblings : stack
          visited <- gets $ Set.member next
          if visited then dfs stack' else do
            modify $ Set.insert next
            case Map.lookup next neighbourMap of
              Just neighbours -> (next :) <$> dfs (neighbours : stack')
              Nothing -> dfs stack'

  in catMaybes
      $ takeWhile isJust
      $ flip evalState Set.empty
      $ sequenceA $ repeat findComponent

unifyComponents :: Sqlite.Database -> Map NodeHash NodeState -> Component -> Component -> IO (Maybe (Int, Edge))
unifyComponents db nodeStates c1 c2 = timed "unifyComponents" $ do
  let unify (origin, destination) =
        findPath db origin destination <&> \path ->
          let pathState = (id &&& flip Map.lookup nodeStates) <$> path
              hiddenCount = length $ filter (isNothing . snd) pathState
              shortenPath path = nonEmpty =<< dropExpanded <$> nonEmpty path
              dropExpanded = NE.filter $ snd >>> (/= Just Expanded)
              edge = shortenPath pathState >>= \p -> do
                let source = fst $ NE.head p
                    target = fst $ NE.last p
                guard $ source /= target
                pure $ Edge source target
          in sequenceA (hiddenCount, edge)
      shortest = listToMaybe . sortOn snd . catMaybes
  fmap shortest $ sequenceA $ fmap unify $ (,) <$> Set.toList c1 <*> Set.toList c2

printComponents :: [Component] -> IO ()
printComponents components = do
  putStrLn "\x1b[1;1H\x1b[2JfindComponents:\n"
  for_ ([ 0 .. ] `zip` components) $ \(i, nodes) -> do
    putStrLn $ "  \x1b[1;" <> show (31 + i `mod` 7) <> "mCOMPONENT:"
    for_ nodes $ \nodeHash -> putStrLn $ "    " <> Text.unpack nodeHash
    putStrLn "\x1b[0m"

fromSQLInt :: Num a => SQLData -> a
fromSQLInt (SQLInteger n) = fromIntegral n

timed :: String -> IO a -> IO a
timed label action = do
  startTime <- Clock.getCurrentTime
  result <- action
  endTime <- Clock.getCurrentTime
  putStrLn $ label <> " took " <> show
    ( Clock.nominalDiffTimeToSeconds
    $ Clock.diffUTCTime endTime startTime
    ) <> " seconds"
  pure result

traceValue :: Show a => String -> a -> a
traceValue label x = trace (label <> " = " <> show x) x
