module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable
  ( InterferenceMutableGraph,
    empty,
    getAllNodes,
    getNode,
    getNodeByIndex,
    getEdges,
    getEdgesByIndex,
    addNode,
    addEdge,
    addEdgeByIndex,
    updateNode,
    updateEdge,
    removeNode,
    removeEdge,
    freeze,
    thaw,
    isEmpty,
    coalesceMove,
    freezeMove,
  )
where

import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base qualified as B (InterferenceGraphEdgeLabel (..), InterferenceGraphNode (..), Move (destination, source), coalesceMove, freezeMove, newInterferenceGraphNode, removeNode)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable qualified as Immutable (InterferenceGraph (..))
import Compiler.Utils.Graph.Base (Edge (..), EdgeIndex (..), GraphException (..), Node (..), NodeIndex (..))
import Data.Primitive.MutVar
import Data.Vector qualified as V
import Data.Vector.Growable qualified as GV
import GHC.Records (HasField (..))
import RIO
import RIO.List qualified as List (sortOn)
import RIO.Map qualified as Map

data MNodeState node edge s = MNodeState {index :: NodeIndex, val :: node, outEdges :: GV.GrowableVector s (Edge edge), inEdges :: GV.GrowableVector s (Edge edge)}

newtype MNode node edge s = MNode (MutVar s (MNodeState node edge s))

data MGraphState var edge s = MGraphState {vertices :: GV.GrowableVector s (MNode (B.InterferenceGraphNode var) edge s), nodeMap :: Map.Map var NodeIndex, edgeIndexCounter :: !Int}

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

getEdges :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> var -> m (Vector (Edge B.InterferenceGraphEdgeLabel))
getEdges mgraph srcN tgtN = do
  graph <- readMGraphVar mgraph
  case (graph.nodeMap Map.!? srcN, graph.nodeMap Map.!? tgtN) of
    (Just srcIndex, Just tgtIndex) -> getEdgesByIndex mgraph srcIndex tgtIndex
    _ -> throwM KeyNotFound

getEdgesByIndex :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> m (Vector (Edge B.InterferenceGraphEdgeLabel))
getEdgesByIndex mgraph srcIndex tgtIndex = do
  srcNode <- getNodeByIndex mgraph srcIndex
  tgtNode <- getNodeByIndex mgraph tgtIndex
  pure $ V.filter (\edge -> edge.target == tgtNode.index) srcNode.outEdges

addNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> m (Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel)
addNode mgraph var = addNodeInternal mgraph (B.newInterferenceGraphNode var [])

addNodeInternal :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> B.InterferenceGraphNode var -> m (Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel)
addNodeInternal mgraph node = do
  graph <- readMGraphVar mgraph
  if any (\var -> isJust $ graph.nodeMap Map.!? var) node.vars
    then throwM KeyAlradyExist
    else do
      index <- NodeIndex <$> GV.length graph.vertices
      newNode <- newMNode index node
      GV.push graph.vertices newNode
      let newNodeMap = foldr (`Map.insert` index) graph.nodeMap node.vars
      writeMGraphVar mgraph graph {nodeMap = newNodeMap}
      freezeMNode newNode

addEdge :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> var -> B.InterferenceGraphEdgeLabel -> m (Edge B.InterferenceGraphEdgeLabel)
addEdge mgraph srcN tgtN label = do
  graph <- readMGraphVar mgraph
  case (graph.nodeMap Map.!? srcN, graph.nodeMap Map.!? tgtN) of
    (Just srcIndex, Just tgtIndex) -> addEdgeByIndex mgraph srcIndex tgtIndex label
    _ -> throwM KeyNotFound

addEdgeByIndex :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> B.InterferenceGraphEdgeLabel -> m (Edge B.InterferenceGraphEdgeLabel)
addEdgeByIndex mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex) label = do
  graph <- readMGraphVar mgraph
  srcNode <- readMNodeVar =<< GV.read graph.vertices srcIndex
  tgtNode <- readMNodeVar =<< GV.read graph.vertices tgtIndex
  let outEdge = Edge (EdgeIndex graph.edgeIndexCounter) (NodeIndex srcIndex) (NodeIndex tgtIndex) label
  GV.push srcNode.outEdges outEdge
  GV.push tgtNode.inEdges outEdge
  when (srcIndex /= tgtIndex) $ do
    let inEdge = Edge (EdgeIndex graph.edgeIndexCounter) (NodeIndex tgtIndex) (NodeIndex srcIndex) label
    GV.push srcNode.inEdges inEdge
    GV.push tgtNode.outEdges inEdge
  writeMGraphVar mgraph $ graph {Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable.edgeIndexCounter = graph.edgeIndexCounter + 1}
  pure outEdge

putEdgeByIndex :: (Ord var, PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> B.InterferenceGraphEdgeLabel -> m (Edge B.InterferenceGraphEdgeLabel)
putEdgeByIndex mgraph srcIndex tgtIndex label = do
  edges <- getEdgesByIndex mgraph srcIndex tgtIndex
  case edges V.!? 0 of
    Nothing -> pure ()
    Just edge -> removeEdge mgraph edge.source edge.target edge.index
  addEdgeByIndex mgraph srcIndex tgtIndex label

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

updateEdge :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> B.InterferenceGraphEdgeLabel -> m ()
updateEdge mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex) edgeIndex val = do
  graph <- readMGraphVar mgraph

  srcNode <- readMNodeVar =<< GV.read graph.vertices srcIndex
  len <- GV.length srcNode.outEdges
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read srcNode.outEdges index
    when (edge.index == edgeIndex) $ GV.write srcNode.outEdges index (edge {val = val} :: Edge B.InterferenceGraphEdgeLabel)

  len <- GV.length srcNode.inEdges
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read srcNode.inEdges index
    when (edge.index == edgeIndex) $ GV.write srcNode.inEdges index (edge {val = val} :: Edge B.InterferenceGraphEdgeLabel)

  when (srcIndex /= tgtIndex) $ do
    tgtNode <- readMNodeVar =<< GV.read graph.vertices tgtIndex
    len <- GV.length tgtNode.inEdges
    forM_ [0 .. len - 1] $ \index -> do
      edge <- GV.read tgtNode.inEdges index
      when (edge.index == edgeIndex) $ GV.write tgtNode.inEdges index (edge {val = val} :: Edge B.InterferenceGraphEdgeLabel)

    len <- GV.length tgtNode.outEdges
    forM_ [0 .. len - 1] $ \index -> do
      edge <- GV.read tgtNode.outEdges index
      when (edge.index == edgeIndex) $ GV.write tgtNode.outEdges index (edge {val = val} :: Edge B.InterferenceGraphEdgeLabel)

removeNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> Node (B.InterferenceGraphNode var) B.InterferenceGraphEdgeLabel -> m ()
removeNode mgraph removedNode = do
  graph <- readMGraphVar mgraph
  len <- GV.length graph.vertices
  let NodeIndex removedIndex = removedNode.index
  newGraph :: InterferenceMutableGraph _ _ <- empty
  forM_ [0 .. len - 1] $ \index ->
    if index == removedIndex
      then pure ()
      else do
        node <- readMNodeVar =<< GV.read graph.vertices index
        let newNode = B.removeNode removedNode.val node.val
        _ <- addNodeInternal newGraph newNode
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
          whenM (V.all (\edge' -> edge.index /= edge'.index) <$> getEdgesByIndex newGraph sourceIndex targetIndex) $ do
            _ <- addEdgeByIndex newGraph sourceIndex targetIndex edge.val
            pure ()

  writeMGraphVar mgraph =<< readMGraphVar newGraph

removeEdge :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> m ()
removeEdge mgraph (NodeIndex srcIndex) (NodeIndex tgtIndex) edgeIndex = do
  graph <- readMGraphVar mgraph

  srcMNode <- GV.read graph.vertices srcIndex
  srcNode <- readMNodeVar srcMNode
  newOutEdges <- GV.new
  len <- GV.length srcNode.outEdges
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read srcNode.outEdges index
    when (edge.index /= edgeIndex) $ GV.push newOutEdges edge
  newInEdges <- GV.new
  len <- GV.length srcNode.inEdges
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read srcNode.inEdges index
    when (edge.index /= edgeIndex) $ GV.push newInEdges edge
  writeMNodeVar srcMNode srcNode {Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable.outEdges = newOutEdges, Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable.inEdges = newInEdges}

  tgtMNode <- GV.read graph.vertices tgtIndex
  tgtNode <- readMNodeVar tgtMNode
  newOutEdges <- GV.new
  len <- GV.length tgtNode.outEdges
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read tgtNode.outEdges index
    when (edge.index /= edgeIndex) $ GV.push newOutEdges edge
  newInEdges <- GV.new
  len <- GV.length tgtNode.inEdges
  forM_ [0 .. len - 1] $ \index -> do
    edge <- GV.read tgtNode.inEdges index
    when (edge.index /= edgeIndex) $ GV.push newInEdges edge
  writeMNodeVar tgtMNode tgtNode {Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable.outEdges = newOutEdges, Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable.inEdges = newInEdges}

readMNodeVar :: PrimMonad m => MNode node edge (PrimState m) -> m (MNodeState node edge (PrimState m))
readMNodeVar (MNode var) = readMutVar var

readMGraphVar :: PrimMonad m => InterferenceMutableGraph var (PrimState m) -> m (MGraphState var B.InterferenceGraphEdgeLabel (PrimState m))
readMGraphVar (InterferenceMutableGraph var) = readMutVar var

writeMNodeVar :: PrimMonad m => MNode node edge (PrimState m) -> MNodeState node edge (PrimState m) -> m ()
writeMNodeVar (MNode var) = writeMutVar var

writeMGraphVar :: PrimMonad m => InterferenceMutableGraph var (PrimState m) -> MGraphState var B.InterferenceGraphEdgeLabel (PrimState m) -> m ()
writeMGraphVar (InterferenceMutableGraph var) = writeMutVar var

newMNode :: PrimMonad m => NodeIndex -> node -> m (MNode node edge (PrimState m))
newMNode index val = do
  outEdges <- GV.new
  inEdges <- GV.new
  MNode <$> newMutVar (MNodeState {index = index, val = val, outEdges = outEdges, inEdges = inEdges})

freezeMNode :: PrimMonad m => MNode (B.InterferenceGraphNode var) edge (PrimState m) -> m (Node (B.InterferenceGraphNode var) edge)
freezeMNode (MNode var) = do
  mn <- readMutVar var
  outEdges <- GV.freeze mn.outEdges
  inEdges <- GV.freeze mn.inEdges
  pure $ Node {index = mn.index, val = mn.val, outEdges = outEdges, inEdges = inEdges}

thawMNode :: PrimMonad m => Node (B.InterferenceGraphNode var) edge -> m (MNode (B.InterferenceGraphNode var) edge (PrimState m))
thawMNode node = do
  outEdges <- GV.thaw node.outEdges
  inEdges <- GV.thaw node.inEdges
  MNode <$> newMutVar (MNodeState {index = node.index, val = node.val, outEdges = outEdges, inEdges = inEdges})

freeze :: PrimMonad m => InterferenceMutableGraph var (PrimState m) -> m (Immutable.InterferenceGraph var)
freeze (InterferenceMutableGraph var) = do
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
  pure $ Immutable.InterferenceGraph vertices graph.edgeIndexCounter graph.nodeMap

thaw :: PrimMonad m => Immutable.InterferenceGraph var -> m (InterferenceMutableGraph var (PrimState m))
thaw graph = do
  vertices <- GV.thaw =<< mapM thawMNode graph.vertices
  InterferenceMutableGraph <$> newMutVar (MGraphState {vertices = vertices, nodeMap = graph.nodeMap, edgeIndexCounter = graph.edgeCount})

isEmpty :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> m Bool
isEmpty = fmap null . getAllNodes

coalesceMove :: (Ord var, PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> B.Move var -> m ()
coalesceMove graph move = do
  source <- getNode graph move.source
  target <- getNode graph move.destination
  let newNode = B.coalesceMove source.val target.val move
  updateNode graph source.index newNode
  forM_ target.outEdges $ \edge -> do
    putEdgeByIndex graph source.index edge.target edge.val
  removeNode graph target

freezeMove :: (Ord var, PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> B.Move var -> m ()
freezeMove graph move = do
  source <- getNode graph move.source
  target <- getNode graph move.destination
  let newSource = B.freezeMove move source.val
  let newTarget = B.freezeMove move target.val
  updateNode graph source.index newSource
  updateNode graph target.index newTarget
  pure ()
