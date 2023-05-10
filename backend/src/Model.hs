{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

data Node = Node
  { nodeData :: Text,
    nodeType :: NodeType
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type NodeType = Text

type NodeHash = Text

data NodeState
  = Collapsed
  | Expanded
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type NodeMap a = Map NodeHash a

data Edge = Edge
  { edgeGroup :: Int,
    edgeSource :: NodeHash,
    edgeTarget :: NodeHash
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type Graph = (NodeMap Node, Set Edge)

exampleGraph :: Graph
exampleGraph =
  ( Map.fromList
      [ ("hash1", (Node "data1" "type1")),
        ("hash2", (Node "data2" "type2"))
      ],
    Set.fromList [Edge 0 "hash1" "hash2"]
  )

-- $> LBSC.putStrLn $ Data.Aeson.encode Model.exampleGraph
