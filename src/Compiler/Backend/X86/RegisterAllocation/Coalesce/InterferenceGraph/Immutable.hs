module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable
  ( InterferenceGraph (..),
    getNode,
    getNodeByIndex,
    getAllNodes,
    getOutNeiborhoodsByIndex,
    getInNeiborhoodsByIndex,
  )
where

import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base (InterferenceGraphEdgeLabel (..), InterferenceGraphNode)
import Compiler.Utils.Graph.Base (Node (..), NodeIndex (..))
import RIO hiding (unlines)
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map
import RIO.Vector.Partial qualified as Vec

data InterferenceGraph var = InterferenceGraph {vertices :: Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel), edgeCount :: !Int, nodeMap :: Map.Map var NodeIndex} deriving (Show)

getNode :: Ord var => InterferenceGraph var -> var -> Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel
getNode graph node =
  let index = graph.nodeMap Map.! node
   in getNodeByIndex graph index

getNodeByIndex :: InterferenceGraph var -> NodeIndex -> Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel
getNodeByIndex graph (NodeIndex index) = graph.vertices Vec.! index

getAllNodes :: InterferenceGraph var -> Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel)
getAllNodes graph = graph.vertices

getOutNeiborhoodsByIndex :: InterferenceGraph var -> NodeIndex -> Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel)
getOutNeiborhoodsByIndex graph index =
  let node = getNodeByIndex graph index
   in getNodeByIndex graph <$> node.outIndexes

getInNeiborhoodsByIndex :: InterferenceGraph var -> NodeIndex -> Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel)
getInNeiborhoodsByIndex graph index =
  let node = getNodeByIndex graph index
   in getNodeByIndex graph <$> node.inIndexes
