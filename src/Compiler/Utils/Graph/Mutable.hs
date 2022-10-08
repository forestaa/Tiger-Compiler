module Compiler.Utils.Graph.Mutable
  ( MGraph,
    MutableGraph (..),
    freeze,
    thaw,
  )
where

import Compiler.Utils.Graph.Base (Directional (Directional, UnDirectional), Edge (..), EdgeIndex (..), GraphException (..), Node (..), NodeIndex (..))
import Compiler.Utils.Graph.Immutable qualified as Immutable (IGraph (..))
import Data.Kind (Type)
import Data.Primitive.MutVar
import Data.Vector qualified as V
import Data.Vector.Growable qualified as GV
import RIO
import RIO.Map qualified as Map

data MNodeState node edge s = MNodeState {index :: NodeIndex, val :: node, outEdges :: GV.GrowableVector s (Edge edge), inEdges :: GV.GrowableVector s (Edge edge)}

newtype MNode node edge s = MNode (MutVar s (MNodeState node edge s))

data MGraphState node edge s = MGraphState {vertices :: GV.GrowableVector s (MNode node edge s), nodeMap :: Map.Map node NodeIndex, edgeIndexCounter :: !Int}

newtype MGraph (d :: Directional) node edge s = MGraph (MutVar s (MGraphState node edge s))

class MutableGraph (graph :: Type -> Type) (d :: Directional) node edge | graph -> d, graph -> node, graph -> edge where
  empty :: PrimMonad m => m (graph (PrimState m))
  getNode :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> node -> m (Node node edge)
  getNodeByIndex :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> NodeIndex -> m (Node node edge)
  getAllNodes :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> m (Vector (Node node edge))
  getEdgesByIndex :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> NodeIndex -> NodeIndex -> m (Vector (Edge edge))
  addNode :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> node -> m (Node node edge)
  addEdge :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> node -> node -> edge -> m (Edge edge)
  addEdgeByIndex :: (PrimMonad m, MonadThrow m) => graph (PrimState m) -> NodeIndex -> NodeIndex -> edge -> m (Edge edge)
  updateNode :: (PrimMonad m, MonadThrow m) => graph (PrimState m) -> NodeIndex -> node -> m ()
  updateEdge :: (PrimMonad m, MonadThrow m) => graph (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> edge -> m ()
  removeNode :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> Node node edge -> m ()
  removeEdge :: (PrimMonad m, MonadThrow m) => graph (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> m ()
  reverse :: (PrimMonad m, MonadThrow m, Ord node) => graph (PrimState m) -> m (graph (PrimState m))

instance MutableGraph (MGraph 'Directional node edge) 'Directional node edge where
  empty :: PrimMonad m => m (MGraph 'Directional node edge (PrimState m))
  empty = do
    vec <- GV.new
    MGraph <$> newMutVar (MGraphState {vertices = vec, nodeMap = Map.empty, edgeIndexCounter = 0})

  getNode :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> node -> m (Node node edge)
  getNode mgraph n = do
    graph <- readMGraphVar mgraph
    case graph.nodeMap Map.!? n of
      Nothing -> throwM KeyNotFound
      Just index -> getNodeByIndex mgraph index

  getNodeByIndex :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> NodeIndex -> m (Node node edge)
  getNodeByIndex mgraph (NodeIndex index) = do
    graph <- readMGraphVar mgraph
    freezeNode =<< GV.read graph.vertices index

  getAllNodes :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> m (Vector (Node node edge))
  getAllNodes mgraph = do
    graph <- readMGraphVar mgraph
    V.mapM freezeNode =<< GV.freeze graph.vertices

  getEdgesByIndex :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> NodeIndex -> NodeIndex -> m (Vector (Edge edge))
  getEdgesByIndex mgraph srcIndex tgtIndex = do
    srcNode <- getNodeByIndex mgraph srcIndex
    tgtNode <- getNodeByIndex mgraph tgtIndex
    pure $ V.filter (\edge -> edge.target == tgtNode.index) srcNode.outEdges

  addNode :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> node -> m (Node node edge)
  addNode mgraph n = do
    graph <- readMGraphVar mgraph
    case graph.nodeMap Map.!? n of
      Just _ -> throwM KeyAlradyExist
      Nothing -> do
        index <- NodeIndex <$> GV.length graph.vertices
        node <- newMNode index n
        GV.push graph.vertices node
        writeMGraphVar mgraph graph {nodeMap = Map.insert n index graph.nodeMap}
        freezeNode node

  addEdge :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> node -> node -> edge -> m (Edge edge)
  addEdge mgraph srcN tgtN label = do
    graph <- readMGraphVar mgraph
    case (graph.nodeMap Map.!? srcN, graph.nodeMap Map.!? tgtN) of
      (Just srcIndex, Just tgtIndex) -> addEdgeByIndex mgraph srcIndex tgtIndex label
      _ -> throwM KeyNotFound

  addEdgeByIndex :: (PrimMonad m, MonadThrow m) => MGraph 'Directional node edge (PrimState m) -> NodeIndex -> NodeIndex -> edge -> m (Edge edge)
  addEdgeByIndex mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex) label = do
    graph <- readMGraphVar mgraph
    srcNode <- readMNodeVar =<< GV.read graph.vertices srcIndex
    tgtNode <- readMNodeVar =<< GV.read graph.vertices tgtIndex
    let edge = Edge (EdgeIndex graph.edgeIndexCounter) (NodeIndex srcIndex) (NodeIndex tgtIndex) label
    GV.push srcNode.outEdges edge
    GV.push tgtNode.inEdges edge
    writeMGraphVar mgraph $ graph {Compiler.Utils.Graph.Mutable.edgeIndexCounter = graph.edgeIndexCounter + 1}
    pure edge

  updateNode :: (PrimMonad m, MonadThrow m) => MGraph 'Directional node edge (PrimState m) -> NodeIndex -> node -> m ()
  updateNode mgraph (NodeIndex index) val = do
    graph <- readMGraphVar mgraph
    mnode <- GV.read graph.vertices index
    node <- readMNodeVar mnode
    let newVertex = node {Compiler.Utils.Graph.Mutable.val = val}
    writeMNodeVar mnode newVertex

  updateEdge :: (PrimMonad m, MonadThrow m) => MGraph 'Directional node edge (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> edge -> m ()
  updateEdge mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex) edgeIndex val = do
    graph <- readMGraphVar mgraph

    srcNode <- readMNodeVar =<< GV.read graph.vertices srcIndex
    len <- GV.length srcNode.outEdges
    forM_ [0 .. len - 1] $ \index -> do
      edge <- GV.read srcNode.outEdges index
      when (edge.index == edgeIndex) $ GV.write srcNode.outEdges index (edge {val = val} :: Edge edge)

    tgtNode <- readMNodeVar =<< GV.read graph.vertices tgtIndex
    len <- GV.length tgtNode.inEdges
    forM_ [0 .. len - 1] $ \index -> do
      edge <- GV.read tgtNode.inEdges index
      when (edge.index == edgeIndex) $ GV.write tgtNode.inEdges index (edge {val = val} :: Edge edge)

  removeNode :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> Node node edge -> m ()
  removeNode mgraph removedNode = do
    graph <- readMGraphVar mgraph
    len <- GV.length graph.vertices
    let NodeIndex removedIndex = removedNode.index
    newGraph :: MGraph 'Directional _ _ _ <- empty
    forM_ [0 .. len - 1] $ \index ->
      if index == removedIndex
        then pure ()
        else do
          node <- readMNodeVar =<< GV.read graph.vertices index
          _ <- addNode newGraph node.val
          pure ()

    let newIndex (NodeIndex i) = if i < removedIndex then NodeIndex i else NodeIndex (i - 1)
    forM_ [0 .. len - 1] $ \index ->
      when (index /= removedIndex) $ do
        node <- readMNodeVar =<< GV.read graph.vertices index

        outEdgeLength <- GV.length node.outEdges
        forM_ [0 .. outEdgeLength - 1] $ \edgeIndex -> do
          edge <- GV.read node.outEdges edgeIndex
          when (edge.target /= removedNode.index) $ do
            let sourceIndex = newIndex edge.source
                targetIndex = newIndex edge.target
            _ <- addEdgeByIndex newGraph sourceIndex targetIndex edge.val
            pure ()

    writeMGraphVar mgraph =<< readMGraphVar newGraph

  removeEdge :: (PrimMonad m, MonadThrow m) => MGraph 'Directional node edge (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> m ()
  removeEdge mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex) edgeIndex = do
    graph <- readMGraphVar mgraph

    srcMNode <- GV.read graph.vertices srcIndex
    srcNode <- readMNodeVar srcMNode
    newOutEdges <- GV.new
    len <- GV.length srcNode.outEdges
    forM_ [0 .. len - 1] $ \index -> do
      edge <- GV.read srcNode.outEdges index
      when (edge.index /= edgeIndex) $ GV.push newOutEdges edge
    writeMNodeVar srcMNode srcNode {Compiler.Utils.Graph.Mutable.outEdges = newOutEdges}

    tgtMNode <- GV.read graph.vertices tgtIndex
    tgtNode <- readMNodeVar tgtMNode
    newInEdges <- GV.new
    len <- GV.length tgtNode.inEdges
    forM_ [0 .. len - 1] $ \index -> do
      edge <- GV.read tgtNode.inEdges index
      when (edge.index /= edgeIndex) $ GV.push newInEdges edge
    writeMNodeVar tgtMNode tgtNode {Compiler.Utils.Graph.Mutable.inEdges = newInEdges}

  reverse :: (PrimMonad m, MonadThrow m, Ord node) => MGraph 'Directional node edge (PrimState m) -> m (MGraph 'Directional node edge (PrimState m))
  reverse graph = do
    newGraph <- empty
    mgraph <- readMGraphVar graph
    len <- GV.length mgraph.vertices
    forM_ [0 .. len - 1] $ \index -> do
      node <- readMNodeVar =<< GV.read mgraph.vertices index
      addNode newGraph node.val

    forM_ [0 .. len - 1] $ \index -> do
      node <- readMNodeVar =<< GV.read mgraph.vertices index

      outEdgesLen <- GV.length node.outEdges
      forM_ [0 .. outEdgesLen - 1] $ \edgeIndex -> do
        edge <- GV.read node.outEdges edgeIndex
        addEdgeByIndex newGraph edge.target edge.source edge.val

    pure newGraph

instance MutableGraph (MGraph 'UnDirectional node edge) 'UnDirectional node edge

readMNodeVar :: PrimMonad m => MNode node edge (PrimState m) -> m (MNodeState node edge (PrimState m))
readMNodeVar (MNode var) = readMutVar var

readMGraphVar :: PrimMonad m => MGraph d node edge (PrimState m) -> m (MGraphState node edge (PrimState m))
readMGraphVar (MGraph var) = readMutVar var

writeMNodeVar :: PrimMonad m => MNode node edge (PrimState m) -> MNodeState node edge (PrimState m) -> m ()
writeMNodeVar (MNode var) = writeMutVar var

writeMGraphVar :: PrimMonad m => MGraph d node edge (PrimState m) -> MGraphState node edge (PrimState m) -> m ()
writeMGraphVar (MGraph var) = writeMutVar var

newMNode :: PrimMonad m => NodeIndex -> node -> m (MNode node edge (PrimState m))
newMNode index val = do
  outEdges <- GV.new
  inEdges <- GV.new
  MNode <$> newMutVar (MNodeState {index = index, val = val, outEdges = outEdges, inEdges = inEdges})

freezeNode :: PrimMonad m => MNode node edge (PrimState m) -> m (Node node edge)
freezeNode (MNode var) = do
  mn <- readMutVar var
  outEdges <- GV.freeze mn.outEdges
  inEdges <- GV.freeze mn.inEdges
  pure $ Node {index = mn.index, val = mn.val, outEdges = outEdges, inEdges = inEdges}

thawNode :: PrimMonad m => Node node edge -> m (MNode node edge (PrimState m))
thawNode node = do
  outEdges <- GV.thaw node.outEdges
  inEdges <- GV.thaw node.inEdges
  MNode <$> newMutVar (MNodeState {index = node.index, val = node.val, outEdges = outEdges, inEdges = inEdges})

freeze :: PrimMonad m => MGraph d node edge (PrimState m) -> m (Immutable.IGraph d node edge)
freeze (MGraph var) = do
  graph <- readMutVar var
  vertices <-
    mapM
      ( \(MNode var) -> do
          node <- readMutVar var
          outEdges <- GV.freeze node.outEdges
          inEdges <- GV.freeze node.inEdges
          pure $ Node {index = node.index, val = node.val, outEdges = outEdges, inEdges = inEdges}
      )
      =<< GV.freeze graph.vertices
  pure $ Immutable.IGraph vertices graph.edgeIndexCounter graph.nodeMap

thaw :: PrimMonad m => Immutable.IGraph d node edge -> m (MGraph d node edge (PrimState m))
thaw graph = do
  vertices <- GV.thaw =<< mapM thawNode graph.vertices
  MGraph <$> newMutVar (MGraphState {vertices = vertices, nodeMap = graph.nodeMap, edgeIndexCounter = graph.edgeIndexCounter})
