{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

data Node = Node
  { nodeData :: Text
  , nodeType :: NodeType
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type NodeType = Text

type NodeHash = Text

data NodeState
  = Collapsed
  | Expanded
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type NodeMap a = Map NodeHash a

data Edge = Edge
  { edgeGroup :: Int
  , edgeSource :: NodeHash
  , edgeTarget :: NodeHash
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type Graph = (NodeMap Node, Set Edge)
