module Compiler.Utils.Graph.Immutable
  ( IGraph (..),
    ImmutableGraph (..),
    bfs,
    isEmpty,
    DebugGraphviz (..),
  )
where

import Compiler.Utils.Graph.Base (Directional (..), Edge (..), Node (..), NodeIndex (..))
import Compiler.Utils.String (unlines)
import Control.Monad.State.Strict
import Data.MultiSet qualified as Multi
import RIO hiding (unlines)
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map
import RIO.Seq (Seq ((:<|), (:|>)))
import RIO.Seq qualified as Seq
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec
import RIO.Vector.Partial qualified as Vec

data IGraph (d :: Directional) node edge = IGraph {vertices :: Vector (Node node edge), edgeCount :: !Int, nodeMap :: Map.Map node NodeIndex} deriving (Show)

instance (Ord node, Ord edge) => Eq (IGraph d node edge) where
  graph1 == graph2 = (vertices graph1 == vertices graph2) && (edges graph1 == edges graph2)
    where
      vertices :: IGraph d node edge -> Set.Set node
      vertices graph = Set.fromList . Vec.toList $ Vec.map (.val) graph.vertices
      edges :: IGraph d node edge -> Multi.MultiSet (node, node, edge)
      edges graph = Multi.fromList . Vec.toList . Vec.map (\edge -> ((getNodeByIndex graph edge.source).val, (getNodeByIndex graph edge.target).val, edge.val)) $ Vec.concatMap (.outEdges) graph.vertices

class ImmutableGraph (d :: Directional) node edge graph | graph -> d, graph -> node, graph -> edge where
  getNode :: (Ord node) => graph -> node -> Node node edge
  getNodeByIndex :: graph -> NodeIndex -> Node node edge
  getAllNodes :: graph -> Vector (Node node edge)
  getOutNeiborhoodsByIndex :: graph -> NodeIndex -> Vector (Node node edge)
  getInNeiborhoodsByIndex :: graph -> NodeIndex -> Vector (Node node edge)

instance ImmutableGraph d node edge (IGraph d node edge) where
  getNode :: (Ord node) => IGraph d node edge -> node -> Node node edge
  getNode graph node =
    let index = graph.nodeMap Map.! node
     in getNodeByIndex graph index

  getNodeByIndex :: IGraph d node edge -> NodeIndex -> Node node edge
  getNodeByIndex graph (NodeIndex index) = graph.vertices Vec.! index

  getAllNodes :: IGraph d node edge -> Vector (Node node edge)
  getAllNodes graph = graph.vertices

  getOutNeiborhoodsByIndex :: IGraph d node edge -> NodeIndex -> Vector (Node node edge)
  getOutNeiborhoodsByIndex graph index =
    let node = getNodeByIndex graph index
     in getNodeByIndex graph <$> node.outIndexes

  getInNeiborhoodsByIndex :: IGraph d node edge -> NodeIndex -> Vector (Node node edge)
  getInNeiborhoodsByIndex graph index =
    let node = getNodeByIndex graph index
     in getNodeByIndex graph <$> node.inIndexes

isEmpty :: forall graph d node edge. (ImmutableGraph d node edge graph) => graph -> Bool
isEmpty = null . getAllNodes

bfs :: forall graph d node edge. (ImmutableGraph d node edge graph) => graph -> [Node node edge] -> [Node node edge]
bfs graph initials = flip evalState Set.empty $ walk (Seq.fromList initials)
  where
    walk :: Seq.Seq (Node node edge) -> State (Set.Set NodeIndex) [Node node edge]
    walk Seq.Empty = pure []
    walk (node :<| queue) = do
      visited <- get
      if Set.member node.index visited
        then walk queue
        else do
          modify $ Set.insert node.index
          fmap (node :) . walk $ Vec.foldl' (:|>) queue (getOutNeiborhoodsByIndex graph node.index)

class DebugGraphviz graph where
  debugGraphviz :: graph -> Text

instance (Display node, Display edge) => DebugGraphviz (IGraph 'Directional node edge) where
  debugGraphviz graph = textDisplay $ "digraph G{\n" <> nodeGraphvizStatements graph <> edgeGraphvizStatements graph <> "}\n"
    where
      nodeGraphvizStatements :: IGraph 'Directional node edge -> Utf8Builder
      nodeGraphvizStatements graph = foldMap (\node -> fold ["  ", display node.val, ";\n"]) $ getAllNodes graph
      edgeGraphvizStatements :: IGraph 'Directional node edge -> Utf8Builder
      edgeGraphvizStatements graph = foldMap (nodeToGraphviz graph) (getAllNodes graph)
        where
          nodeToGraphviz :: IGraph 'Directional node edge -> Node node edge -> Utf8Builder
          nodeToGraphviz graph node = unlines . Vec.toList $ edgeToGraphviz node <$> getOutNeiborhoodsByIndex graph node.index
          edgeToGraphviz :: Node node edge -> Node node edge -> Utf8Builder
          edgeToGraphviz src tgt = fold ["  ", display src.val, " -> ", display tgt.val, ";"]

instance (Display node, Display edge) => DebugGraphviz (IGraph 'UnDirectional node edge) where
  debugGraphviz graph = textDisplay $ "graph G{\n" <> nodeGraphvizStatements graph <> edgeGraphvizStatements graph <> "}\n"
    where
      nodeGraphvizStatements :: IGraph 'UnDirectional node edge -> Utf8Builder
      nodeGraphvizStatements graph = foldMap (\node -> fold ["  ", display node.val, ";\n"]) $ getAllNodes graph
      edgeGraphvizStatements :: IGraph 'UnDirectional node edge -> Utf8Builder
      edgeGraphvizStatements graph = foldMap (nodeToGraphviz graph) (getAllNodes graph)
        where
          nodeToGraphviz :: IGraph 'UnDirectional node edge -> Node node edge -> Utf8Builder
          nodeToGraphviz graph node = unlines . Vec.toList . Vec.mapMaybe (edgeToGraphviz node) $ getOutNeiborhoodsByIndex graph node.index
          edgeToGraphviz :: Node node edge -> Node node edge -> Maybe Utf8Builder
          edgeToGraphviz src tgt
            | src.index <= tgt.index = Just $ fold ["  ", display src.val, " -- ", display tgt.val, ";"]
            | otherwise = Nothing
