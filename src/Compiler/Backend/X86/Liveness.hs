module Compiler.Backend.X86.Liveness
  ( ControlFlows (ControlFlows),
    ControlFlow (..),
    InterferenceGraph,
    InterferenceMutableGraph,
    InterferenceGraphEdgeLabel (..),
    thaw,
    freeze,
    buildInterfereceGraph,
  )
where

import Compiler.Backend.X86.Arch (Label)
import Compiler.Utils.Graph.Base (Directional (..), Edge, EdgeIndex, Node (..), NodeIndex)
import Compiler.Utils.Graph.Immutable qualified as Immutable (IGraph, ImmutableGraph (..), bfs)
import Compiler.Utils.Graph.Mutable qualified as Mutable (MGraph, MutableGraph (..), freeze, thaw)
import GHC.Records (HasField (..))
import RIO hiding (reverse)
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map ((!))
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec

data ControlFlows var val = ControlFlows {flows :: [ControlFlow var val], vars :: Set.Set var}

data ControlFlow var val
  = Instruction {src :: [var], dst :: [var], val :: val}
  | Move {src :: [var], dst :: [var], val :: val}
  | Jump {jumps :: [Label], val :: val}
  | CJump {jumps :: [Label], val :: val}
  | Label {label' :: Label, val :: val}
  deriving (Show, Eq, Ord)

instance HasField "sources" (ControlFlow var val) [var] where
  getField Instruction {src} = src
  getField Move {src} = src
  getField _ = []

instance HasField "destinations" (ControlFlow var val) [var] where
  getField Instruction {dst} = dst
  getField Move {dst} = dst
  getField _ = []

instance HasField "jumpsTo" (ControlFlow var val) [Label] where
  getField Jump {jumps} = jumps
  getField CJump {jumps} = jumps
  getField _ = []

isMove :: ControlFlow var val -> Bool
isMove Move {} = True
isMove _ = False

isJump :: ControlFlow var val -> Bool
isJump Jump {} = True
isJump CJump {} = True
isJump _ = False

doesGoNext :: ControlFlow var val -> Bool
doesGoNext Jump {} = False
doesGoNext _ = True

isLabel :: ControlFlow var val -> Bool
isLabel Label {} = True
isLabel _ = False

data ControlFlowNode var val = ControlFlowNode {key :: Int, definedVariables :: Set.Set var, usedVariables :: Set.Set var, isMove :: Bool, val :: ControlFlow var val} deriving (Show)

instance Eq (ControlFlowNode var val) where
  node1 == node2 = node1.key == node2.key

instance Ord (ControlFlowNode var val) where
  node1 <= node2 = node1.key <= node2.key

newControlFlowNode :: Ord var => ControlFlow var val -> Int -> ControlFlowNode var val
newControlFlowNode flow key = ControlFlowNode {key = key, definedVariables = Set.fromList flow.destinations, usedVariables = Set.fromList flow.sources, isMove = isMove flow, val = flow}

newtype ControlFlowGraph var val = ControlFlowGraph (Immutable.IGraph 'Directional (ControlFlowNode var val) ()) deriving (Show, Immutable.ImmutableGraph 'Directional (ControlFlowNode var val) ())

data InterferenceGraphEdgeLabel = InterferenceGraphEdgeLabel deriving (Show, Eq, Ord)

newtype InterferenceGraph var = InterferenceGraph (Immutable.IGraph 'UnDirectional var InterferenceGraphEdgeLabel) deriving (Show, Eq, Immutable.ImmutableGraph 'UnDirectional var InterferenceGraphEdgeLabel)

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

  updateNode :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> var -> m ()
  updateNode (InterferenceMutableGraph graph) = Mutable.updateNode graph

  updateEdge :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph var (PrimState m) -> NodeIndex -> NodeIndex -> Compiler.Utils.Graph.Base.EdgeIndex -> InterferenceGraphEdgeLabel -> m ()
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

newControlFlowGraph :: Ord var => [ControlFlow var val] -> ControlFlowGraph var val
newControlFlowGraph flows =
  ControlFlowGraph
    ( runST $ do
        graph <- Mutable.empty
        let nodes = zipWith newControlFlowNode flows [0 ..]
            labelMap = makeLabelMap nodes
        forM_ nodes $ Mutable.addNode graph
        addControlFlowGraphEdges graph labelMap nodes
        Mutable.freeze graph
    )
  where
    makeLabelMap :: [ControlFlowNode var val] -> Map.Map Label (ControlFlowNode var val)
    makeLabelMap nodes = Map.fromList . fmap (\node -> (node.val.label', node)) $ filter (\node -> isLabel node.val) nodes
    addControlFlowGraphEdges :: (PrimMonad m, MonadThrow m, Mutable.MutableGraph d (ControlFlowNode var val) () graph) => graph (PrimState m) -> Map.Map Label (ControlFlowNode var val) -> [ControlFlowNode var val] -> m ()
    addControlFlowGraphEdges _ _ [] = pure ()
    addControlFlowGraphEdges graph labelMap [node] =
      when (isJump node.val) $
        forM_ node.val.jumpsTo $
          \label -> Mutable.addEdge graph node (labelMap Map.! label) ()
    addControlFlowGraphEdges graph labelMap (node1 : node2 : nodes) = do
      when (isJump node1.val) $
        forM_ node1.val.jumpsTo $
          \label -> Mutable.addEdge graph node1 (labelMap Map.! label) ()
      when (doesGoNext node1.val) . void $
        Mutable.addEdge graph node1 node2 ()
      addControlFlowGraphEdges graph labelMap (node2 : nodes)

data LiveVariables var val = LiveVariable {node :: ControlFlowNode var val, input :: Set.Set var, output :: Set.Set var} deriving (Show, Eq)

newLiveVariables :: ControlFlowNode var val -> LiveVariables var val
newLiveVariables node = LiveVariable node Set.empty Set.empty

type LiveVariablesMap var val = Map.Map (ControlFlowNode var val) (LiveVariables var val)

newLiveVariablesMap :: ControlFlowGraph var val -> LiveVariablesMap var val
newLiveVariablesMap graph = Vec.foldl' (\liveVariablesMap node -> Map.insert node.val (newLiveVariables node.val) liveVariablesMap) Map.empty $ Immutable.getAllNodes graph

solveDataFlowEquation :: (Ord var) => ControlFlowGraph var val -> LiveVariablesMap var val
solveDataFlowEquation graph = flip runReader graph . solveDataFlowEquationLoop $ newLiveVariablesMap graph
  where
    solveDataFlowEquationLoop :: (Ord var, MonadReader (ControlFlowGraph var val) m) => LiveVariablesMap var val -> m (LiveVariablesMap var val)
    solveDataFlowEquationLoop liveVariablesMap = do
      liveVariablesMap' <- solveDataFlowEquationStep liveVariablesMap
      if liveVariablesMap == liveVariablesMap'
        then pure liveVariablesMap'
        else solveDataFlowEquationLoop liveVariablesMap'

solveDataFlowEquationStep :: (Ord var, MonadReader (ControlFlowGraph var val) m) => LiveVariablesMap var val -> m (LiveVariablesMap var val)
solveDataFlowEquationStep liveVariables = do
  ControlFlowGraph graph <- ask
  let Just last = Vec.find (\node -> Vec.null node.outEdges && not (Vec.null node.inEdges)) (Immutable.getAllNodes graph)
      reversedGraph = runST $ Mutable.thaw graph >>= Mutable.reverse >>= Mutable.freeze
  foldM
    ( \liveVariables node -> do
        newLiveVariable <- dataFlowEquation liveVariables node
        pure $ Map.insert node.val newLiveVariable liveVariables
    )
    liveVariables
    $ Immutable.bfs reversedGraph [last]

dataFlowEquation :: (Ord var, MonadReader (ControlFlowGraph var val) m) => LiveVariablesMap var val -> Node (ControlFlowNode var val) () -> m (LiveVariables var val)
dataFlowEquation variablesMap node = do
  graph <- ask
  let variables = variablesMap Map.! node.val
      output = Set.unions $ Vec.map (\target -> (variablesMap Map.! target.val).input) (Immutable.getOutNeiborhoodsByIndex graph node.index)
      input = node.val.usedVariables `Set.union` (output Set.\\ node.val.definedVariables)
  pure variables {input = input, output = output}

newInterferenceGraph :: Ord var => Set.Set var -> LiveVariablesMap var val -> InterferenceGraph var
newInterferenceGraph vars liveVariablesMap =
  InterferenceGraph
    ( runST $ do
        graph <- Mutable.empty
        forM_ vars $ Mutable.addNode graph
        forM_ liveVariablesMap $ addInterferenceGraphEdges graph
        Mutable.freeze graph
    )
  where
    addInterferenceGraphEdges :: (PrimMonad m, Ord var, MonadThrow m) => Mutable.MGraph 'UnDirectional var InterferenceGraphEdgeLabel (PrimState m) -> LiveVariables var val -> m ()
    addInterferenceGraphEdges graph liveVariables
      | liveVariables.node.isMove =
          let notUsedOutput = liveVariables.output Set.\\ liveVariables.node.usedVariables
           in sequence_ $ addEdge <$> Set.toList liveVariables.node.definedVariables <*> Set.toList notUsedOutput
      | otherwise =
          let notDefinedOutput = liveVariables.output Set.\\ liveVariables.node.definedVariables
           in sequence_ $ addEdge <$> Set.toList liveVariables.node.definedVariables <*> Set.toList notDefinedOutput
      where
        addEdge src tgt =
          whenM (Vec.null <$> Mutable.getEdges graph src tgt) . void $
            Mutable.addEdge graph src tgt InterferenceGraphEdgeLabel

buildInterfereceGraph :: Ord var => ControlFlows var val -> InterferenceGraph var
buildInterfereceGraph flows = newInterferenceGraph flows.vars . solveDataFlowEquation $ newControlFlowGraph flows.flows
