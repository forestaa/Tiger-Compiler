module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable
  ( InterferenceGraph (..),
    getNode,
    getNodeByIndex,
    getAllNodes,
    getOutNeiborhoods,
    getOutNeiborhoodsByIndex,
    isEmpty,
  )
where

import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base (InterferenceGraphEdgeLabel (..), InterferenceGraphNode (moves), Move (destination, source))
import Compiler.Utils.Graph.Base (Edge (..), Node (..), NodeIndex (..))
import Compiler.Utils.Graph.Immutable (DebugGraphviz (debugGraphviz))
import Compiler.Utils.String (unlines)
import Data.MultiSet qualified as Multi
import RIO hiding (unlines)
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec
import RIO.Vector.Partial qualified as Vec

-- | no inEdges to save memory
data InterferenceGraph var = InterferenceGraph {vertices :: Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel), edgeCount :: !Int, nodeMap :: Map.Map var NodeIndex} deriving (Show)

instance (Ord var) => Eq (InterferenceGraph var) where
  graph1 == graph2 = (vertices graph1 == vertices graph2) && (edges graph1 == edges graph2)
    where
      vertices :: InterferenceGraph var -> Set.Set (InterferenceGraphNode var)
      vertices graph = Set.fromList . Vec.toList $ Vec.map (.val) graph.vertices
      edges :: InterferenceGraph var -> Multi.MultiSet (InterferenceGraphNode var, InterferenceGraphNode var, InterferenceGraphEdgeLabel)
      edges graph = Multi.fromList . Vec.toList . Vec.map (\edge -> ((getNodeByIndex graph edge.source).val, (getNodeByIndex graph edge.target).val, edge.val)) $ Vec.concatMap (.outEdges) graph.vertices

getNode :: (Ord var) => InterferenceGraph var -> var -> Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel
getNode graph node =
  let index = graph.nodeMap Map.! node
   in getNodeByIndex graph index

getNodeByIndex :: InterferenceGraph var -> NodeIndex -> Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel
getNodeByIndex graph (NodeIndex index) = graph.vertices Vec.! index

getAllNodes :: InterferenceGraph var -> Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel)
getAllNodes graph = graph.vertices

getOutNeiborhoods :: (Ord var) => InterferenceGraph var -> var -> Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel)
getOutNeiborhoods graph var = getOutNeiborhoodsByIndex graph (graph.nodeMap Map.! var)

getOutNeiborhoodsByIndex :: InterferenceGraph var -> NodeIndex -> Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel)
getOutNeiborhoodsByIndex graph index =
  let node = getNodeByIndex graph index
   in getNodeByIndex graph <$> node.outIndexes

isEmpty :: InterferenceGraph var -> Bool
isEmpty = null . getAllNodes

instance (Display var, Ord var) => DebugGraphviz (InterferenceGraph var) where
  debugGraphviz graph = textDisplay $ "graph G{\n" <> nodeGraphvizStatements graph <> edgeGraphvizStatements graph <> "}\n"
    where
      nodeGraphvizStatements :: InterferenceGraph var -> Utf8Builder
      nodeGraphvizStatements graph = foldMap (\node -> fold ["  ", display node.val, ";\n"]) $ getAllNodes graph
      edgeGraphvizStatements :: (Ord var) => InterferenceGraph var -> Utf8Builder
      edgeGraphvizStatements graph = foldMap (nodeToGraphviz graph) (getAllNodes graph)
        where
          nodeToGraphviz :: (Ord var) => InterferenceGraph var -> Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel -> Utf8Builder
          nodeToGraphviz graph node = unlines $ (edgeToGraphviz node <$> getOutNeiborhoodsByIndex graph node.index) Vec.++ Vec.fromList ((\move -> moveToGraphviz (getNode graph move.source) (getNode graph move.destination) move.isCoalesceable) <$> Set.toList node.val.moves)
          edgeToGraphviz :: Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel -> Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel -> Utf8Builder
          edgeToGraphviz src tgt = fold ["  ", display src.val, " -- ", display tgt.val, ";"]
          moveToGraphviz :: Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel -> Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel -> Bool -> Utf8Builder
          moveToGraphviz src tgt coalesceable = fold ["  ", display src.val, " -- ", display tgt.val, " [label = ", bool "false" "true" coalesceable, " style=\"dotted\"];"]
