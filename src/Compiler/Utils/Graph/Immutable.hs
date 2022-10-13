module Compiler.Utils.Graph.Immutable (IGraph (..), ImmutableGraph (..), bfs) where

import Compiler.Utils.Graph.Base (Directional (..), Node (..), NodeIndex (..))
import Control.Monad.State.Strict
import RIO (Int, Ord, Vector, flip, pure, ($), (.), (<$>))
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map
import RIO.Seq (Seq ((:<|), (:|>)))
import RIO.Seq qualified as Seq
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec (foldl')
import RIO.Vector.Partial qualified as Vec

data IGraph (d :: Directional) node edge = IGraph {vertices :: Vector (Node node edge), edgeCount :: !Int, nodeMap :: Map.Map node NodeIndex}

class ImmutableGraph (d :: Directional) node edge graph | graph -> d, graph -> node, graph -> edge where
  getNode :: Ord node => graph -> node -> Node node edge
  getNodeByIndex :: graph -> NodeIndex -> Node node edge
  getAllNodes :: graph -> Vector (Node node edge)
  getOutNeiborhoodsByIndex :: graph -> NodeIndex -> Vector (Node node edge)
  getInNeiborhoodsByIndex :: graph -> NodeIndex -> Vector (Node node edge)

instance ImmutableGraph d node edge (IGraph d node edge) where
  getNode :: Ord node => IGraph d node edge -> node -> Node node edge
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

bfs :: forall graph d node edge. ImmutableGraph d node edge graph => graph -> [Node node edge] -> [Node node edge]
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
