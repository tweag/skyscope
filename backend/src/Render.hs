{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render where

import Common
import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS.Class
import Control.Monad.State (State, evalState)
import Data.Foldable (for_)
import Data.Functor (($>), (<&>))
import Data.HList (Label (..))
import Data.HList.Record (HasField, hLookupByLabel)
import Data.List (nub, sortOn, tails)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import Data.Traversable (for)
import Database.SQLite3 (SQLData (..))
import Model
import Query (HasFindPathMemo, findPath)
import Sqlite (Database)
import qualified Sqlite
import System.Exit (ExitCode (..))
import System.Process.Text (readProcessWithExitCode)
import Prelude

data RenderResult = RenderResult
  { renderOutput :: LazyText.Text,
    renderTimePathFinding :: Int,
    renderTimeRendering :: Int,
    renderTimeTotal :: Int
  }

renderGraph ::
  (HasUnifyComponentsMemo r, HasFindPathMemo r) =>
  Database ->
  NodeMap NodeState ->
  Memoize r RenderResult
renderGraph database nodeStates = do
  -- TODO: memoize rendered svgs

  let selectEdges :: NodeHash -> Text -> IO [[SQLData]]
      selectEdges nodeHash whereClause =
        Sqlite.executeSql
          database
          [ "SELECT group_num, s.hash, t.hash FROM edge",
            "INNER JOIN node AS s ON s.idx = edge.source",
            "INNER JOIN node AS t ON t.idx = edge.target",
            "WHERE " <> whereClause <> ";"
          ]
          [SQLText nodeHash]
  edges <- fmap (nub . concat) $
    flip Map.traverseWithKey nodeStates $
      \nodeHash state ->
        liftIO $
          mconcat
            [ selectEdges nodeHash "s.hash = ?1",
              selectEdges nodeHash "t.hash = ?1"
            ]
            <&> ( >>=
                    \[SQLInteger group, SQLText source, SQLText target] ->
                      let collapsed = state == Collapsed
                          hidden = (`Map.notMember` nodeStates)
                       in if collapsed && (hidden source || hidden target)
                            then []
                            else [Edge (fromIntegral group) source target]
                )

  let nodeIdentityMap :: NodeMap NodeHash
      nodeIdentityMap =
        Map.fromSet id $
          Set.fromList $
            Map.keys nodeStates
              <> (edges >>= \(Edge _ source target) -> [source, target])
  nodeMap <- for nodeIdentityMap $ \nodeHash ->
    liftIO $
      Sqlite.executeSql
        database
        ["SELECT type, data FROM node WHERE hash = ?;"]
        [SQLText nodeHash]
        <&> \[[SQLText nodeType, SQLText nodeData]] -> Node nodeType nodeData

  links <- findPathLinks database nodeStates edges

  let graph =
        Text.unlines
          [ "digraph \"\" {",
            "    pad=2",
            "    node"
              <> graphvizAttributes
                [ ("color", "#efefef"),
                  ("penwidth", "0.2"),
                  ("style", "filled,rounded")
                ],
            "    edge"
              <> graphvizAttributes
                [ ("arrowsize", "0.5"),
                  ("color", "#3f3f3f"),
                  ("penwidth", "0.2")
                ],
            Text.unlines $ uncurry graphvizNode <$> Map.assocs nodeMap,
            Text.unlines $ graphvizEdge [] <$> edges,
            Text.unlines $
              let tooltip hiddenCount = "Click here to show " <> Text.pack (show hiddenCount) <> " hidden nodes."
               in links <&> \PathLink {..} ->
                    graphvizEdge
                      [ ("arrowhead", "none"),
                        ("class", "Path"),
                        ("label", "Open"),
                        ("labeltooltip", tooltip linkHidden),
                        ("penwidth", "1.0"),
                        ("style", "dotted"),
                        ("tooltip", tooltip linkHidden)
                      ]
                      linkEdge,
            "}"
          ]

  renderOutput <- liftIO $ do
    Text.writeFile "/tmp/skyscope.dot" graph -- For debugging
    readProcessWithExitCode "dot" ["-Tsvg"] graph <&> \case
      (ExitFailure code, _, err) -> error $ "dot exit " <> show code <> ": " <> Text.unpack err
      (ExitSuccess, svg, _) -> LazyText.fromStrict svg

  pure $ RenderResult renderOutput 0 0 0
  where
    graphvizNode :: NodeHash -> Node -> Text
    graphvizNode nodeHash (Node nodeType nodeData) =
      let truncatedNodeData = Text.take 8192 nodeData
          label = nodeType <> "\\n" <> truncatedNodeData
          nodeState = Map.lookup nodeHash nodeStates
          hidden = nodeState == Nothing
       in "    node_" <> nodeHash
            <> graphvizAttributes
              [ ("tooltip", truncatedNodeData),
                ("class", Text.pack $ fromMaybe "" $ show <$> nodeState),
                ("shape", if hidden then "point" else "box"),
                ("height", if hidden then "0.2" else "0.6"),
                ("width", if hidden then "0.2" else "3.0"),
                ("fixedsize", "true"),
                ("label", label),
                ("id", nodeHash)
              ]

    graphvizEdge :: [(Text, Text)] -> Edge -> Text
    graphvizEdge attrs (Edge _ source target) =
      "    node_" <> source <> " -> node_" <> target
        <> graphvizAttributes (attrs ++ [("id", source <> "_" <> target)])

    graphvizAttributes :: [(Text, Text)] -> Text
    graphvizAttributes attrs =
      let f (name, value) = name <> "=\"" <> value <> "\""
       in " [ " <> Text.intercalate "; " (f <$> attrs) <> " ];"

data PathLink = PathLink
  { linkHidden :: Int,
    linkLength :: Int,
    linkEdge :: Edge
  }
  deriving (Eq, Ord, Show)

findPathLinks ::
  (HasUnifyComponentsMemo r, HasFindPathMemo r) =>
  Database ->
  NodeMap NodeState ->
  [Edge] ->
  Memoize r [PathLink]
findPathLinks database nodeStates edges =
  timed "findPathLinks" $
    nub <$> do
      let components = findComponents edges nodeStates
          pairs = concat [[(c1, c2), (c2, c1)] | c1 : cs <- tails components, c2 <- cs]
      fmap catMaybes $ for pairs $ uncurry $ unifyComponents database nodeStates

type Component = NodeMap NodeState

findComponents :: [Edge] -> NodeMap NodeState -> [Component]
findComponents edges nodes =
  let discreteMap = nodes $> []
      neighbourMap =
        flip Map.union discreteMap $
          Map.fromAscListWith (++) $
            sortOn fst $
              edges >>= \(Edge _ s t) ->
                case (,) <$> Map.lookup s nodes <*> Map.lookup t nodes of
                  Just (source, target) ->
                    [ (s, [(t, target)]),
                      (t, [(s, source)])
                    ]
                  Nothing -> []

      findComponent :: State (NodeMap NodeState) (Maybe Component)
      findComponent = do
        unvisitedNodes <- gets $ Map.difference nodes
        case Map.lookupMin unvisitedNodes of
          Nothing -> pure Nothing
          Just node ->
            fmap Just $
              Map.fromList
                <$> dfs [[node]] <&> (`Map.intersection` nodes)

      dfs = \case
        [] -> pure []
        [] : stack -> dfs stack
        (next@(node, value) : siblings) : stack -> do
          let stack' = siblings : stack
          visited <- gets $ Map.member node
          if visited
            then dfs stack'
            else do
              modify $ Map.insert node value
              case Map.lookup node neighbourMap of
                Just neighbours -> (next :) <$> dfs (neighbours : stack')
                Nothing -> dfs stack'
   in catMaybes $
        takeWhile isJust $
          flip evalState Map.empty $
            sequenceA $ repeat findComponent

type UnifyComponentsMemo = TVar (Map (Component, Component) (Maybe PathLink))

type HasUnifyComponentsMemo r = HasField "unifyComponents" r UnifyComponentsMemo

getUnifyComponentsMemo :: HasUnifyComponentsMemo r => r -> UnifyComponentsMemo
getUnifyComponentsMemo = hLookupByLabel (Label :: Label "unifyComponents")

unifyComponents ::
  (HasUnifyComponentsMemo r, HasFindPathMemo r) =>
  Database ->
  NodeMap NodeState ->
  Component ->
  Component ->
  Memoize r (Maybe PathLink)
unifyComponents database nodeStates = curry $
  memoize "unifyComponents" getUnifyComponentsMemo $ \(c1, c2) -> do
    let unify (origin, destination) =
          findPath database origin destination <&> \path ->
            let pathState = (id &&& flip Map.lookup nodeStates) <$> path
                hiddenCount = length $ filter (isNothing . snd) pathState
                shortenPath path = nonEmpty =<< dropExpanded <$> nonEmpty path
                dropExpanded = NE.filter $ snd >>> (/= Just Expanded)
             in shortenPath pathState >>= \p -> do
                  let source = fst $ NE.head p
                      target = fst $ NE.last p
                  guard $ source /= target
                  pure $
                    PathLink
                      { linkHidden = hiddenCount,
                        linkLength = NE.length p,
                        linkEdge = Edge 0 source target
                      }

        shortest = listToMaybe . sortOn linkLength . catMaybes
    fmap shortest $ sequenceA $ fmap unify $ (,) <$> Map.keys c1 <*> Map.keys c2

printComponents :: [Component] -> IO ()
printComponents components = do
  putStrLn "findComponents:\n"
  for_ ([0 ..] `zip` components) $ \(i, nodes) -> do
    putStrLn $ "  \x1b[1;" <> show (31 + i `mod` 7 :: Integer) <> "mCOMPONENT:"
    for_ (Map.assocs nodes) $ \(nodeHash, nodeState) ->
      putStrLn $ "    " <> Text.unpack nodeHash <> " " <> show nodeState
    putStrLn "\x1b[0m"
