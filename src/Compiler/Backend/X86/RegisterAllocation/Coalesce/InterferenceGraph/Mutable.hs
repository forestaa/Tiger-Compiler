module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable
  ( InterferenceMutableGraph,
    empty,
    getAllNodes,
    getNode,
    getNodeByIndex,
    getEdge,
    getEdgeByIndex,
    addNode,
    addEdge,
    addEdgeByIndex,
    updateNode,
    removeNode,
    removeEdge,
    freeze,
    thaw,
    isEmpty,
    addMove,
    constrainMove,
    coalesceMove,
    freezeMove,
  )
where

import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base qualified as B (InterferenceGraphEdgeLabel (..), InterferenceGraphNode (..), Move (destination, source), addMove, coalesceMove, constrainMove, freezeMove, newInterferenceGraphNode, removeNode)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable qualified as Immutable (InterferenceGraph (..))
import Compiler.Utils.Graph.Base (Edge (..), EdgeIndex (..), GraphException (..), Node (..), NodeIndex (..))
import Data.Primitive.MutVar
import Data.Vector qualified as V
import Data.Vector.Growable qualified as GV
import RIO
import RIO.List qualified as List
import RIO.Map qualified as Map

-- | no inEdges to save memory
data MNodeState node edge s = MNodeState {index :: NodeIndex, val :: node, outEdges :: GV.GrowableVector s (Edge edge)}

newtype MNode node edge s = MNode (MutVar s (MNodeState node edge s))

data MGraphState var edge s = MGraphState {vertices :: GV.GrowableVector s (MNode (B.InterferenceGraphNode var) edge s), nodeMap :: Map.Map var NodeIndex, edgeIndexCounter :: !Int}

-- | no inEdges to save memory
newtype InterferenceMutableGraph var s = InterferenceMutableGraph (MutVar s (MGraphState var B.InterferenceGraphEdgeLabel s))

empty :: PrimMonad m => m (InterferenceMutableGraph var (PrimState m))
empty = do
  vec <- GV.new
  InterferenceMutableGraph <$> newMutVar (MGraphState {vertices = vec, nodeMap = Map.empty, edgeIndexCounter = 0})

getNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> m (Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel)
getNode mgraph n = do
  graph <- readMGraphVar mgraph
  case graph.nodeMap Map.!? n of
    Nothing -> throwM KeyNotFound
    Just index -> getNodeByIndex mgraph index

getNodeByIndex :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> m (Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel)
getNodeByIndex mgraph (NodeIndex index) = do
  graph <- readMGraphVar mgraph
  freezeMNode =<< GV.read graph.vertices index

getAllNodes :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> m (Vector (Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel))
getAllNodes mgraph = do
  graph <- readMGraphVar mgraph
  V.mapM freezeMNode =<< GV.freeze graph.vertices

getEdge :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> var -> m (Maybe (Edge B.InterferenceGraphEdgeLabel))
getEdge mgraph srcN tgtN = do
  graph <- readMGraphVar mgraph
  case (graph.nodeMap Map.!? srcN, graph.nodeMap Map.!? tgtN) of
    (Just srcIndex, Just tgtIndex) -> getEdgeByIndex mgraph srcIndex tgtIndex
    _ -> throwM KeyNotFound

getEdgeByIndex :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> m (Maybe (Edge B.InterferenceGraphEdgeLabel))
getEdgeByIndex mgraph srcIndex tgtIndex = do
  srcNode <- getNodeByIndex mgraph srcIndex
  tgtNode <- getNodeByIndex mgraph tgtIndex
  pure $ V.find (\edge -> edge.target == tgtNode.index) srcNode.outEdges

addNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> m (Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel)
addNode mgraph var = do
  graph <- readMGraphVar mgraph
  let node = B.newInterferenceGraphNode var []
  if any (\var -> isJust $ graph.nodeMap Map.!? var) node.vars
    then throwM KeyAlradyExist
    else do
      index <- NodeIndex <$> GV.length graph.vertices
      newNode <- newMNode index node
      GV.push graph.vertices newNode
      let newNodeMap = foldr (`Map.insert` index) graph.nodeMap node.vars
      writeMGraphVar mgraph graph {nodeMap = newNodeMap}
      freezeMNode newNode

addEdge :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> var -> m (Edge B.InterferenceGraphEdgeLabel)
addEdge mgraph srcN tgtN = do
  graph <- readMGraphVar mgraph
  case (graph.nodeMap Map.!? srcN, graph.nodeMap Map.!? tgtN) of
    (Just srcIndex, Just tgtIndex) -> addEdgeByIndex mgraph srcIndex tgtIndex
    _ -> throwM KeyNotFound

addEdgeByIndex :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> m (Edge B.InterferenceGraphEdgeLabel)
addEdgeByIndex mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex) = do
  edge <- getEdgeByIndex mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex)
  case edge of
    Just edge -> pure edge
    Nothing -> do
      srcNode <- readMNode mgraph (NodeIndex srcIndex)
      tgtNode <- readMNode mgraph (NodeIndex tgtIndex)
      graph <- readMGraphVar mgraph
      let outEdge = Edge (EdgeIndex graph.edgeIndexCounter) (NodeIndex srcIndex) (NodeIndex tgtIndex) B.InterferenceGraphEdgeLabel
      GV.push srcNode.outEdges outEdge
      when (srcIndex /= tgtIndex) $ do
        let inEdge = Edge (EdgeIndex graph.edgeIndexCounter) (NodeIndex tgtIndex) (NodeIndex srcIndex) B.InterferenceGraphEdgeLabel
        GV.push tgtNode.outEdges inEdge
      writeMGraphVar mgraph $ graph {Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable.edgeIndexCounter = graph.edgeIndexCounter + 1}
      pure outEdge

updateNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> B.InterferenceGraphNode var -> m ()
updateNode mgraph (NodeIndex index) newNode = do
  graph <- readMGraphVar mgraph
  mnode <- GV.read graph.vertices index
  node <- readMNodeVar mnode
  let newVertex = node {Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable.val = newNode}
      newNodeMap = foldr (\var nodeMap -> Map.insert var (NodeIndex index) $ Map.delete var nodeMap) graph.nodeMap newNode.vars
      newGraph = graph {nodeMap = newNodeMap}
  writeMNodeVar mnode newVertex
  writeMGraphVar mgraph newGraph

removeNode :: forall m var. (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel -> m ()
removeNode mgraph removedNode = do
  indexes <- List.delete removedNode.index <$> getAllIndexes mgraph
  nodeMapVar <- newMutVar Map.empty
  forM_ indexes $ \index -> do
    node <- readMNode mgraph index
    let newNode = B.removeNode removedNode.val node.val
        index' = newIndex index
    updateRelatedEdges node
    writeMNode mgraph index' (node {index = index', val = newNode} :: MNodeState (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel (PrimState m))
    forM_ newNode.vars $ \var -> modifyMutVar' nodeMapVar $ Map.insert var index'

  graph <- readMGraphVar mgraph
  _ <- GV.pop graph.vertices
  nodeMap <- readMutVar nodeMapVar
  writeMGraphVar mgraph $ graph {nodeMap = nodeMap}
  pure ()
  where
    newIndex :: NodeIndex -> NodeIndex
    newIndex (NodeIndex i) =
      let NodeIndex removedIndex = removedNode.index
       in if i < removedIndex then NodeIndex i else NodeIndex (i - 1)
    updateRelatedEdges :: (PrimMonad m) => MNodeState (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel (PrimState m) -> m ()
    updateRelatedEdges node = do
      edgeLength <- GV.length node.outEdges
      removedEdgesVar <- newMutVar 0
      forM_ [0 .. edgeLength - 1] $ \edgeIndex -> do
        edge <- GV.read node.outEdges edgeIndex
        if edge.target == removedNode.index
          then modifyMutVar' removedEdgesVar $ (+) 1
          else do
            let newEdge = edge {source = newIndex edge.source, target = newIndex edge.target}
            removedEdgesLength <- readMutVar removedEdgesVar
            GV.write node.outEdges (edgeIndex - removedEdgesLength) newEdge
      removedEdgesLength <- readMutVar removedEdgesVar
      forM_ [1 .. removedEdgesLength] $ \_ -> GV.pop node.outEdges

removeEdge :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> Edge B.InterferenceGraphEdgeLabel -> m ()
removeEdge mgraph removedEdge = do
  srcNode <- readMNode mgraph removedEdge.source
  len <- GV.length srcNode.outEdges
  afterRemovedVar <- newMutVar False
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read srcNode.outEdges index
    afterRemoved <- readMutVar afterRemovedVar
    if
        | edge.index == removedEdge.index -> writeMutVar afterRemovedVar True
        | afterRemoved -> GV.write srcNode.outEdges (index - 1) edge
        | otherwise -> pure ()
  _ <- GV.pop srcNode.outEdges

  tgtNode <- readMNode mgraph removedEdge.target
  len <- GV.length tgtNode.outEdges
  afterRemovedVar <- newMutVar False
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read tgtNode.outEdges index
    afterRemoved <- readMutVar afterRemovedVar
    if
        | edge.index == removedEdge.index -> writeMutVar afterRemovedVar True
        | afterRemoved -> GV.write tgtNode.outEdges (index - 1) edge
        | otherwise -> pure ()
  _ <- GV.pop tgtNode.outEdges

  pure ()

readMNodeVar :: PrimMonad m => MNode node edge (PrimState m) -> m (MNodeState node edge (PrimState m))
readMNodeVar (MNode var) = readMutVar var

readMGraphVar :: PrimMonad m => InterferenceMutableGraph var (PrimState m) -> m (MGraphState var B.InterferenceGraphEdgeLabel (PrimState m))
readMGraphVar (InterferenceMutableGraph var) = readMutVar var

writeMNodeVar :: PrimMonad m => MNode node edge (PrimState m) -> MNodeState node edge (PrimState m) -> m ()
writeMNodeVar (MNode var) = writeMutVar var

writeMGraphVar :: PrimMonad m => InterferenceMutableGraph var (PrimState m) -> MGraphState var B.InterferenceGraphEdgeLabel (PrimState m) -> m ()
writeMGraphVar (InterferenceMutableGraph var) = writeMutVar var

readMNode :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> m (MNodeState (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel (PrimState m))
readMNode (InterferenceMutableGraph var) (NodeIndex index) = do
  graph <- readMutVar var
  mnode <- GV.read graph.vertices index
  readMNodeVar mnode

writeMNode :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> MNodeState (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel (PrimState m) -> m ()
writeMNode (InterferenceMutableGraph var) (NodeIndex index) node = do
  graph <- readMutVar var
  mnode <- GV.read graph.vertices index
  writeMNodeVar mnode node

newMNode :: PrimMonad m => NodeIndex -> node -> m (MNode node edge (PrimState m))
newMNode index val = do
  outEdges <- GV.new
  MNode <$> newMutVar (MNodeState {index = index, val = val, outEdges = outEdges})

freezeMNode :: PrimMonad m => MNode (B.InterferenceGraphNode var) edge (PrimState m) -> m (Node (B.InterferenceGraphNode var) edge)
freezeMNode (MNode var) = do
  mn <- readMutVar var
  outEdges <- GV.freeze mn.outEdges
  pure $ Node {index = mn.index, val = mn.val, outEdges = outEdges, inEdges = V.empty}

thawMNode :: PrimMonad m => Node (B.InterferenceGraphNode var) edge -> m (MNode (B.InterferenceGraphNode var) edge (PrimState m))
thawMNode node = do
  outEdges <- GV.thaw node.outEdges
  MNode <$> newMutVar (MNodeState {index = node.index, val = node.val, outEdges = outEdges})

freeze :: PrimMonad m => InterferenceMutableGraph var (PrimState m) -> m (Immutable.InterferenceGraph var)
freeze (InterferenceMutableGraph var) = do
  graph <- readMutVar var
  vertices <-
    mapM
      ( \(MNode var) -> do
          node <- readMutVar var
          outEdges <- GV.freeze node.outEdges
          pure $ Node {index = node.index, val = node.val, outEdges = outEdges, inEdges = V.empty}
      )
      =<< GV.freeze graph.vertices
  pure $ Immutable.InterferenceGraph vertices graph.edgeIndexCounter graph.nodeMap

thaw :: PrimMonad m => Immutable.InterferenceGraph var -> m (InterferenceMutableGraph var (PrimState m))
thaw graph = do
  vertices <- GV.thaw =<< mapM thawMNode graph.vertices
  InterferenceMutableGraph <$> newMutVar (MGraphState {vertices = vertices, nodeMap = graph.nodeMap, edgeIndexCounter = graph.edgeCount})

size :: (PrimMonad m) => InterferenceMutableGraph var (PrimState m) -> m Int
size mgraph = do
  graph <- readMGraphVar mgraph
  GV.length graph.vertices

isEmpty :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> m Bool
isEmpty = fmap null . getAllNodes

getAllIndexes :: (PrimMonad m) => InterferenceMutableGraph var (PrimState m) -> m [NodeIndex]
getAllIndexes mgraph = do
  len <- size mgraph
  pure $ NodeIndex <$> [0 .. len - 1]

addMove :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> B.Move var -> m ()
addMove graph move = do
  src <- getNode graph move.source
  tgt <- getNode graph move.destination
  let srcVal = B.addMove move src.val
      tgtVal = B.addMove move tgt.val
  updateNode graph src.index srcVal
  updateNode graph tgt.index tgtVal

constrainMove :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> B.Move var -> m ()
constrainMove graph move = do
  source <- getNode graph move.source
  target <- getNode graph move.destination
  let srcVal = B.constrainMove move source.val
      tgtVal = B.constrainMove move target.val
  updateNode graph source.index srcVal
  updateNode graph target.index tgtVal

coalesceMove :: (Ord var, PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> B.Move var -> m ()
coalesceMove graph move = do
  source <- getNode graph move.source
  target <- getNode graph move.destination
  let newNode = B.coalesceMove source.val target.val move
  updateNode graph source.index newNode
  forM_ target.outEdges $ \edge -> do
    addEdgeByIndex graph source.index edge.target
  removeNode graph target
  forM_ newNode.getCoalesceableMoves $ \move -> do
    edge <- getEdge graph move.source move.destination
    when (isJust edge) $ constrainMove graph move

freezeMove :: (Ord var, PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> B.Move var -> m ()
freezeMove graph move = do
  source <- getNode graph move.source
  target <- getNode graph move.destination
  let newSource = B.freezeMove move source.val
  let newTarget = B.freezeMove move target.val
  updateNode graph source.index newSource
  updateNode graph target.index newTarget
  pure ()
