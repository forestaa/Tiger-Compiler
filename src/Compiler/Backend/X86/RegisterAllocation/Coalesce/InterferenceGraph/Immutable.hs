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
import Compiler.Utils.Graph.Base (Edge (..), Node (..), NodeIndex (..))
import Data.MultiSet qualified as Multi
import GHC.Records (HasField (getField))
import RIO hiding (unlines)
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec
import RIO.Vector.Partial qualified as Vec

data InterferenceGraph var = InterferenceGraph {vertices :: Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel), edgeCount :: !Int, nodeMap :: Map.Map var NodeIndex} deriving (Show)

instance (Ord var) => Eq (InterferenceGraph var) where
  graph1 == graph2 = (vertices graph1 == vertices graph2) && (edges graph1 == edges graph2)
    where
      vertices :: InterferenceGraph var -> Set.Set (InterferenceGraphNode var)
      vertices graph = Set.fromList . Vec.toList $ Vec.map (getField @"val") graph.vertices
      edges :: InterferenceGraph var -> Multi.MultiSet (InterferenceGraphNode var, InterferenceGraphNode var, InterferenceGraphEdgeLabel)
      edges graph = Multi.fromList . Vec.toList . Vec.map (\edge -> ((getNodeByIndex graph edge.source).val, (getNodeByIndex graph edge.target).val, edge.val)) $ Vec.concatMap (getField @"outEdges") graph.vertices

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
