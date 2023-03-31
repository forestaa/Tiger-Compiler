module Compiler.Backend.X86.RegisterAllocation.InterferenceGraph
  ( InterferenceGraph,
    InterferenceMutableGraph,
    InterferenceGraphEdgeLabel (..),
    thaw,
    freeze,
    newInterferenceGraph,
    buildInterfereceGraph,
  )
where

import Compiler.Backend.X86.Liveness (ControlFlowNode (..))
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), ControlFlowGraph, ControlFlowNode (..), newControlFlowGraph, solveDataFlowEquation)
import Compiler.Utils.Graph.Base (Directional (..), Edge, EdgeIndex, Node (..), NodeIndex)
import Compiler.Utils.Graph.Immutable qualified as Immutable (DebugGraphviz, IGraph, ImmutableGraph (..))
import Compiler.Utils.Graph.Mutable qualified as Mutable (MGraph, MutableGraph (..), freeze, thaw)
import RIO
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec

data InterferenceGraphEdgeLabel = InterferenceGraphEdgeLabel deriving (Show, Eq, Ord)

instance Display InterferenceGraphEdgeLabel where
  display = displayShow

newtype InterferenceGraph var = InterferenceGraph (Immutable.IGraph 'UnDirectional var InterferenceGraphEdgeLabel) deriving (Show, Eq, Immutable.ImmutableGraph 'UnDirectional var InterferenceGraphEdgeLabel, Immutable.DebugGraphviz)

newtype InterferenceMutableGraph var s = InterferenceMutableGraph (Mutable.MGraph 'UnDirectional var InterferenceGraphEdgeLabel s)

instance Mutable.MutableGraph 'UnDirectional var InterferenceGraphEdgeLabel (InterferenceMutableGraph var) where
  empty :: PrimMonad m => m (InterferenceMutableGraph var (PrimState m))
  empty = InterferenceMutableGraph <$> Mutable.empty

  getNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> m (Node var InterferenceGraphEdgeLabel)
  getNode (InterferenceMutableGraph graph) = Mutable.getNode graph

  getNodeByIndex :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> m (Node var InterferenceGraphEdgeLabel)
  getNodeByIndex (InterferenceMutableGraph graph) = Mutable.getNodeByIndex graph

  getAllNodes :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> m (Vector (Node var InterferenceGraphEdgeLabel))
  getAllNodes (InterferenceMutableGraph graph) = Mutable.getAllNodes graph

  getEdges :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> var -> m (Vector (Edge InterferenceGraphEdgeLabel))
  getEdges (InterferenceMutableGraph graph) = Mutable.getEdges graph

  getEdgesByIndex :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> m (Vector (Edge InterferenceGraphEdgeLabel))
  getEdgesByIndex (InterferenceMutableGraph graph) = Mutable.getEdgesByIndex graph

  addNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> m (Node var InterferenceGraphEdgeLabel)
  addNode (InterferenceMutableGraph graph) = Mutable.addNode graph

  addEdge :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> var -> var -> InterferenceGraphEdgeLabel -> m (Edge InterferenceGraphEdgeLabel)
  addEdge (InterferenceMutableGraph graph) = Mutable.addEdge graph

  addEdgeByIndex :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> InterferenceGraphEdgeLabel -> m (Edge InterferenceGraphEdgeLabel)
  addEdgeByIndex (InterferenceMutableGraph graph) = Mutable.addEdgeByIndex graph

  updateNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> var -> m ()
  updateNode (InterferenceMutableGraph graph) = Mutable.updateNode graph

  updateEdge :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> InterferenceGraphEdgeLabel -> m ()
  updateEdge (InterferenceMutableGraph graph) = Mutable.updateEdge graph

  removeNode :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> Node var InterferenceGraphEdgeLabel -> m ()
  removeNode (InterferenceMutableGraph graph) = Mutable.removeNode graph

  removeEdge :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> m ()
  removeEdge (InterferenceMutableGraph graph) = Mutable.removeEdge graph

  reverse :: (PrimMonad m, MonadThrow m, Ord var) => InterferenceMutableGraph var (PrimState m) -> m (InterferenceMutableGraph var (PrimState m))
  reverse (InterferenceMutableGraph graph) = InterferenceMutableGraph <$> Mutable.reverse graph

freeze :: PrimMonad m => InterferenceMutableGraph var (PrimState m) -> m (InterferenceGraph var)
freeze (InterferenceMutableGraph graph) = InterferenceGraph <$> Mutable.freeze graph

thaw :: PrimMonad m => InterferenceGraph var -> m (InterferenceMutableGraph var (PrimState m))
thaw (InterferenceGraph graph) = InterferenceMutableGraph <$> Mutable.thaw graph

newInterferenceGraph :: Ord var => Set.Set var -> L.ControlFlowGraph var val -> InterferenceGraph var
newInterferenceGraph vars cfGraph =
  runST $ do
    graph <- Mutable.empty
    forM_ vars $ Mutable.addNode graph
    forM_ ((.val) <$> Immutable.getAllNodes cfGraph) $ addInterferenceGraphEdges graph
    freeze graph
  where
    addInterferenceGraphEdges :: (PrimMonad m, Ord var, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> L.ControlFlowNode var val -> m ()
    addInterferenceGraphEdges graph cnode
      | cnode.isMove =
          let notUsedOutput = cnode.liveOutVariables Set.\\ cnode.usedVariables Set.\\ cnode.definedVariables
           in sequence_ $ addEdge <$> Set.toList cnode.definedVariables <*> Set.toList notUsedOutput
      | otherwise =
          let notDefinedOutput = cnode.liveOutVariables Set.\\ cnode.definedVariables
           in sequence_ $ addEdge <$> Set.toList cnode.definedVariables <*> Set.toList notDefinedOutput
      where
        addEdge src tgt =
          whenM (Vec.null <$> Mutable.getEdges graph src tgt) . void $
            Mutable.addEdge graph src tgt InterferenceGraphEdgeLabel

buildInterfereceGraph :: (Ord var) => Set.Set var -> [L.ControlFlow var val] -> InterferenceGraph var
buildInterfereceGraph vars = newInterferenceGraph vars . L.solveDataFlowEquation . L.newControlFlowGraph
