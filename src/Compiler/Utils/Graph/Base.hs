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
import RIO (Eq (..), Exception, Int, Ord, Show, Typeable, Vector, fmap, length)

data Directional
  = -- | Both outEdges and inEdges are defined
    Directional
  | -- | forall node1,node2. node1 -> node2 impllies node2 -> node1
    UnDirectional

newtype NodeIndex = NodeIndex Int
  deriving (Eq, Ord, Show)

data Edge edge = Edge {index :: EdgeIndex, source :: NodeIndex, target :: NodeIndex, val :: edge}
  deriving (Show)

newtype EdgeIndex = EdgeIndex Int deriving (Eq, Ord, Show)

data Node node edge = Node {index :: NodeIndex, val :: node, outEdges :: Vector (Edge edge), inEdges :: Vector (Edge edge)}
  deriving (Show)

instance Eq (Node node edge) where
  node1 == node2 = node1.index == node2.index

instance Eq (Edge edge) where
  edge1 == edge2 = edge1.index == edge2.index

instance HasField "outIndexes" (Node node edge) (Vector NodeIndex) where
  getField node = fmap (.target) node.outEdges

instance HasField "inIndexes" (Node node edge) (Vector NodeIndex) where
  getField node = fmap (.target) node.inEdges

instance HasField "outDegree" (Node node edge) Int where
  getField node = length node.outEdges

instance HasField "inDegree" (Node node edge) Int where
  getField node = length node.inEdges

data GraphException = KeyAlradyExist | KeyNotFound deriving (Typeable, Show, Eq)

instance Exception GraphException
