module Compiler.Utils.Graph.Base
  ( Directional (..),
    NodeIndex (NodeIndex),
    EdgeIndex (EdgeIndex),
    Edge (Edge, index, source, target, val),
    Node (Node, index, val, outEdges, inEdges),
    GraphException (..),
  )
where

import GHC.Records (HasField (..))
import RIO (Eq, Exception, Int, Ord, Show, Typeable, Vector, fmap)

data Directional
  = -- | Both outEdges and inEdges are defined
    Directional
  | -- | forall node1,node2. node1 -> node2 impllies node2 -> node1
    UnDirectional

newtype NodeIndex = NodeIndex Int deriving (Eq, Ord, Show)

-- TODO: maybe we need source edge index
newtype EdgeIndex = EdgeIndex Int deriving (Eq, Ord, Show)

data Edge edge = Edge {index :: EdgeIndex, source :: NodeIndex, target :: NodeIndex, val :: edge}
  deriving (Eq, Show)

data Node node edge = Node {index :: NodeIndex, val :: node, outEdges :: Vector (Edge edge), inEdges :: Vector (Edge edge)}
  deriving (Eq, Show)

instance HasField "outIndexes" (Node node edge) (Vector NodeIndex) where
  getField node = fmap (\edge -> edge.target) node.outEdges

instance HasField "inIndexes" (Node node edge) (Vector NodeIndex) where
  getField node = fmap (\edge -> edge.target) node.inEdges

data GraphException = KeyAlradyExist | KeyNotFound deriving (Typeable, Show, Eq)

instance Exception GraphException
