module Compiler.Utils.Graph
  ( HasKey (..),
    Node (val, inDegree, outDegree),
    Edge (src, dst, val),
    Graph,
    MGraph,
    UnLabelledGraph,
    MUnLabelledGraph,
    emptyGraph,
    addNode,
    addEdge,
    addUndirectedEdge,
    removeNode,
    removeEdge,
    freeze,
    thaw,
    newGraph,
  )
where

import Data.Vector.Mutable qualified as MV
import GHC.Records (HasField)
import RIO
import RIO.Map qualified as Map
import RIO.Vector qualified as V

class HasKey node where
  getKey :: node -> String

-- labelled, directed, mutable

type UnLabelledGraph node = Graph () node

type MUnLabelledGraph node = MGraph () node

data Node node = Node {index :: Int, val :: node, inDegree :: Int, outDegree :: Int}

data Edge label node = Edge {index :: Int, src :: Node node, dst :: Node node, val :: label}

data Graph label node = Graph

data MGraph label node = MGraph

instance HasField "vertices" (Graph label node) [Node node]

instance HasField "vertices" (MGraph label node) [Node node]

-- data Graph s edge node = Graph {adjacencyGraph :: MV.MVector s (Maybe edge), verticeIndexes :: Map.Map String Int, nodeCount :: Int, edgeCount :: Int}

emptyGraph :: PrimMonad m => m (MGraph label node)
emptyGraph = undefined

-- TODO: handle existed node
addNode :: PrimMonad m => MGraph label node -> node -> m ()
addNode = undefined

-- TODO: handle existed edge
addEdge :: PrimMonad m => MGraph label node -> node -> node -> label -> m ()
addEdge = undefined

addUndirectedEdge :: PrimMonad m => MGraph label node -> node -> node -> label -> m ()
addUndirectedEdge = undefined

getNode :: HasKey node => MGraph label node -> node -> Maybe (Node node)
getNode = undefined

getEdge :: HasKey node => MGraph label node -> node -> node -> Maybe (Edge label node)
getEdge = undefined

updateNode :: HasKey node => MGraph label node -> node -> m ()
updateNode = undefined

updateEdge :: HasKey node => MGraph label node -> node -> node -> label -> m ()
updateEdge = undefined

putNode :: HasKey node => MGraph label node -> node -> m ()
putNode = undefined

putEdge :: HasKey node => MGraph label node -> node -> node -> label -> m ()
putEdge = undefined

removeNode :: PrimMonad m => MGraph edge node -> Node node -> m ()
removeNode = undefined

removeEdge :: PrimMonad m => MGraph edge node -> Edge label node -> m ()
removeEdge = undefined

removeUndirectedEdge :: PrimMonad m => MGraph label node -> Edge label node -> m ()
removeUndirectedEdge = undefined

freeze :: PrimMonad m => MGraph label node -> m (Graph label node)
freeze = undefined

thaw :: PrimMonad m => Graph label node -> m (MGraph label node)
thaw = undefined

newGraph :: [node] -> [(node, node, label)] -> Graph label node
newGraph _ _ = undefined

getOutgoingEdges :: PrimMonad m => MGraph label node -> node -> [Edge label node]
getOutgoingEdges = undefined

getIncomingEdges :: PrimMonad m => MGraph label node -> node -> [Edge label node]
getIncomingEdges = undefined

getNeiborhoods :: PrimMonad m => MGraph edge node -> node -> m [Node node]
getNeiborhoods = undefined
