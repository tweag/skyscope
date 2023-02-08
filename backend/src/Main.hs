{-# OPTIONS_GHC -fwarn-unused-imports #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (FromJSON, FromJSONKey, FromJSONKeyFunction(..), ToJSON, ToJSONKey)
import qualified Data.Aeson as Json
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Attoparsec.Combinator as Parser
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Coerce (coerce)
import Data.FileEmbed (embedFile, embedFileIfExists)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (nub)
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
  Sqlite.batchInsert db "node" [ "hash", "type", "data" ] $
    Map.assocs nodes <&> \(NodeHash hash, Node nodeType nodeData) ->
      SQLText <$> [ hash, nodeType, nodeType <> ":" <> nodeData ]
  Sqlite.batchInsert db "edge" [ "source", "target" ] $
    Set.toList edges <&> \(Edge (NodeHash source) (NodeHash target)) ->
      SQLText <$> [ source, target ]
  where
    createSchema db = Sqlite.executeStatements db
      [ [ "CREATE TABLE node ("
        , "  hash TEXT PRIMARY KEY,"
        , "  type TEXT,"
        , "  data TEXT"
        , ");"
        ]
      , [ "CREATE TABLE edge ("
        , "  source TEXT,"
        , "  target TEXT,"
        , "  PRIMARY KEY (source, target),"
        , "  FOREIGN KEY (source) REFERENCES node(hash),"
        , "  FOREIGN KEY (target) REFERENCES node(hash)"
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
      --indexJs <- liftIO $ Text.decodeUtf8 <$> BS.readFile "/home/ben/git/skyscope/bazel-bin/frontend/index.js"
      indexJs <- liftIO $ Text.decodeUtf8 <$> BS.readFile "/home/ben/git/skyscope/frontend/index.js"
      styleCss <- liftIO $ Text.decodeUtf8 <$> BS.readFile "/home/ben/git/skyscope/frontend/src/theme.css"
      Web.html $ LazyText.fromStrict $ Text.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "  <head>"
        , "    <title>Skyscope</title>"
        , "    <meta charset=\"UTF-8\">"
        , "    <script>"
        ,       indexJs
        --,       Text.decodeUtf8 $ fromMaybe "" $(embedFileIfExists $(do
        --          found <- TH.runIO $ find (pure True) (filePath ~~? "**/frontend/index.js") "."
        --          pure $ TH.LitE $ TH.StringL $ case found of
        --            [ path ] -> path
        --            [] -> ""
        --        ))
        , "    </script>"
        , "    <style>"
        ,       styleCss
        , "    </style>"
        , "  </head>"
        , "  <body></body>"
        , "</html>"
        ]
    Web.post "/find" $ Json.eitherDecode <$> Web.body >>= \case
      Right pattern -> do
        --liftIO $ threadDelay 5000000
        Web.json =<< liftIO (findNodes db 100 pattern)
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

renderSvg :: Sqlite.Database -> Map NodeHash NodeState -> IO LazyText.Text
renderSvg db nodeStates = do
  edges <- fmap (nub . concat) $ flip Map.traverseWithKey nodeStates $
    \(NodeHash hash) state -> Sqlite.executeSql db
      [ "SELECT source, target FROM edge WHERE source = ?1 OR target = ?1;" ]
      [ SQLText hash ] <&> (>>= (\[ SQLText s, SQLText t ] ->
        let source = NodeHash s
            target = NodeHash t
            collapsed = state == Collapsed
            hidden  = (`Map.notMember` nodeStates)
        in if collapsed && (hidden source || hidden target)
            then [] else [ Edge (NodeHash s) (NodeHash t) ]))
  let nodeIdentityMap = Map.fromList $ map (join (,)) $ Map.keys nodeStates <>
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

{-

      CallStack (from HasCallStack):
        error, called at backend/src/Main.hs:261:35 in main:Main
      dot exit 1: Error: <stdin>: syntax error in line 130 scanning a quoted string (missing endquote? longer than 16384?)


-}
