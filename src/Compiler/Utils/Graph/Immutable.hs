module Compiler.Utils.Graph.Immutable where

import Compiler.Utils.Graph.Base (Directional (..), Node (..), NodeIndex (..))
import Control.Monad.State.Strict
import RIO (Int, Ord, Vector, flip, pure, ($), (<$>))
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map
import RIO.Seq (Seq ((:<|), (:|>)))
import RIO.Seq qualified as Seq
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec (foldl')
import RIO.Vector.Partial qualified as Vec

data IGraph (d :: Directional) node edge = IGraph {vertices :: Vector (Node node edge), edgeIndexCounter :: !Int, nodeMap :: Map.Map node NodeIndex}

class ImmutableGraph graph (d :: Directional) node edge | graph -> d, graph -> node, graph -> edge where
  getNode :: (Ord node) => graph -> node -> Node node edge
  getNodeByIndex :: graph -> NodeIndex -> Node node edge
  getAllNodes :: graph -> Vector (Node node edge)
  getOutNeiborhoodsByIndex :: graph -> NodeIndex -> Vector (Node node edge)
  getInNeiborhoodsByIndex :: graph -> NodeIndex -> Vector (Node node edge)

instance ImmutableGraph (IGraph d node edge) d node edge where
  getNode graph node =
    let index = graph.nodeMap Map.! node
     in getNodeByIndex graph index

  getNodeByIndex graph (NodeIndex index) = graph.vertices Vec.! index

  getAllNodes graph = graph.vertices

  getOutNeiborhoodsByIndex graph index =
    let node = getNodeByIndex graph index
     in getNodeByIndex graph <$> node.outIndexes

  getInNeiborhoodsByIndex graph index =
    let node = getNodeByIndex graph index
     in getNodeByIndex graph <$> node.inIndexes

bfs :: forall graph d node edge. ImmutableGraph graph d node edge => graph -> [Node node edge] -> [Node node edge]
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
          walk $ Vec.foldl' (:|>) queue (getOutNeiborhoodsByIndex graph node.index)
