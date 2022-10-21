module Compiler.Backend.X86.Liveness
  ( ControlFlow (..),
    ControlFlowNode (..),
    ControlFlowGraph,
    newControlFlowNode,
    newControlFlowGraph,
    solveDataFlowEquation,
    freeze,
    thaw,
  )
where

import Compiler.Backend.X86.Arch (Label)
import Compiler.Utils.Graph.Base (Directional (..), Edge (..), EdgeIndex, Node (..), NodeIndex)
import Compiler.Utils.Graph.Immutable qualified as Immutable (IGraph, ImmutableGraph (..), bfs)
import Compiler.Utils.Graph.Mutable qualified as Mutable (MGraph, MutableGraph (..), freeze, thaw)
import GHC.Records (HasField (..))
import RIO hiding (reverse)
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map ((!))
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec

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

data ControlFlowNode var val = ControlFlowNode {key :: Int, definedVariables :: Set.Set var, usedVariables :: Set.Set var, isMove :: Bool, liveInVariables :: Set.Set var, liveOutVariables :: Set.Set var, val :: ControlFlow var val} deriving (Show)

instance Eq (ControlFlowNode var val) where
  node1 == node2 = node1.key == node2.key

instance Ord (ControlFlowNode var val) where
  node1 <= node2 = node1.key <= node2.key

newControlFlowNode :: Ord var => ControlFlow var val -> Int -> ControlFlowNode var val
newControlFlowNode flow key = ControlFlowNode {key = key, definedVariables = Set.fromList flow.destinations, usedVariables = Set.fromList flow.sources, isMove = isMove flow, liveInVariables = Set.empty, liveOutVariables = Set.empty, val = flow}

newtype ControlFlowGraph var val = ControlFlowGraph (Immutable.IGraph 'Directional (ControlFlowNode var val) ())
  deriving (Show, Eq, Immutable.ImmutableGraph 'Directional (ControlFlowNode var val) ())

newtype ControlFlowMutableGraph var val s = ControlFlowMutableGraph (Mutable.MGraph 'Directional (ControlFlowNode var val) () s)

instance Mutable.MutableGraph 'Directional (ControlFlowNode var val) () (ControlFlowMutableGraph var val) where
  empty :: PrimMonad m => m (ControlFlowMutableGraph var val (PrimState m))
  empty = ControlFlowMutableGraph <$> Mutable.empty

  getNode :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> ControlFlowNode var val -> m (Node (ControlFlowNode var val) ())
  getNode (ControlFlowMutableGraph graph) = Mutable.getNode graph

  getNodeByIndex :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> NodeIndex -> m (Node (ControlFlowNode var val) ())
  getNodeByIndex (ControlFlowMutableGraph graph) = Mutable.getNodeByIndex graph

  getAllNodes :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> m (Vector (Node (ControlFlowNode var val) ()))
  getAllNodes (ControlFlowMutableGraph graph) = Mutable.getAllNodes graph

  getEdges :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> ControlFlowNode var val -> ControlFlowNode var val -> m (Vector (Edge ()))
  getEdges (ControlFlowMutableGraph graph) = Mutable.getEdges graph

  getEdgesByIndex :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> NodeIndex -> NodeIndex -> m (Vector (Edge ()))
  getEdgesByIndex (ControlFlowMutableGraph graph) = Mutable.getEdgesByIndex graph

  addNode :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> ControlFlowNode var val -> m (Node (ControlFlowNode var val) ())
  addNode (ControlFlowMutableGraph graph) = Mutable.addNode graph

  addEdge :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> ControlFlowNode var val -> ControlFlowNode var val -> () -> m (Edge ())
  addEdge (ControlFlowMutableGraph graph) = Mutable.addEdge graph

  addEdgeByIndex :: (PrimMonad m, MonadThrow m) => ControlFlowMutableGraph var val (PrimState m) -> NodeIndex -> NodeIndex -> () -> m (Edge ())
  addEdgeByIndex (ControlFlowMutableGraph graph) = Mutable.addEdgeByIndex graph

  updateNode :: (PrimMonad m, MonadThrow m) => ControlFlowMutableGraph var val (PrimState m) -> NodeIndex -> ControlFlowNode var val -> m ()
  updateNode (ControlFlowMutableGraph graph) = Mutable.updateNode graph

  updateEdge :: (PrimMonad m, MonadThrow m) => ControlFlowMutableGraph var val (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> () -> m ()
  updateEdge (ControlFlowMutableGraph graph) = Mutable.updateEdge graph

  removeNode :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> Node (ControlFlowNode var val) () -> m ()
  removeNode (ControlFlowMutableGraph graph) = Mutable.removeNode graph

  removeEdge :: (PrimMonad m, MonadThrow m) => ControlFlowMutableGraph var val (PrimState m) -> NodeIndex -> NodeIndex -> EdgeIndex -> m ()
  removeEdge (ControlFlowMutableGraph graph) = Mutable.removeEdge graph

  reverse :: (PrimMonad m, MonadThrow m, Ord (ControlFlowNode var val)) => ControlFlowMutableGraph var val (PrimState m) -> m (ControlFlowMutableGraph var val (PrimState m))
  reverse (ControlFlowMutableGraph graph) = ControlFlowMutableGraph <$> Mutable.reverse graph

freeze :: PrimMonad m => ControlFlowMutableGraph var val (PrimState m) -> m (ControlFlowGraph var val)
freeze (ControlFlowMutableGraph graph) = ControlFlowGraph <$> Mutable.freeze graph

thaw :: PrimMonad m => ControlFlowGraph var val -> m (ControlFlowMutableGraph var val (PrimState m))
thaw (ControlFlowGraph graph) = ControlFlowMutableGraph <$> Mutable.thaw graph

newControlFlowGraph :: Ord var => [ControlFlow var val] -> ControlFlowGraph var val
newControlFlowGraph flows = runST $ do
  graph <- Mutable.empty
  let nodes = zipWith newControlFlowNode flows [0 ..]
      labelMap = makeLabelMap nodes
  forM_ nodes $ Mutable.addNode graph
  addControlFlowGraphEdges graph labelMap nodes
  freeze graph
  where
    makeLabelMap :: [ControlFlowNode var val] -> Map.Map Label (ControlFlowNode var val)
    makeLabelMap nodes = Map.fromList [(node.val.label', node) | node <- nodes, isLabel node.val]
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

solveDataFlowEquation :: (Ord var) => ControlFlowGraph var val -> ControlFlowGraph var val
solveDataFlowEquation graph =
  let graph' = solveDataFlowEquationStep graph
      liveVariables = (\node -> (node.val.key, node.val.liveInVariables, node.val.liveOutVariables)) <$> Immutable.getAllNodes graph
      liveVariables' = (\node -> (node.val.key, node.val.liveInVariables, node.val.liveOutVariables)) <$> Immutable.getAllNodes graph'
   in if liveVariables == liveVariables'
        then graph'
        else solveDataFlowEquation graph'

solveDataFlowEquationStep :: (Ord var) => ControlFlowGraph var val -> ControlFlowGraph var val
solveDataFlowEquationStep graph =
  let terminals = Vec.toList $ Vec.filter (\node -> Vec.null node.outEdges && not (Vec.null node.inEdges)) (Immutable.getAllNodes graph)
   in runST $ do
        mgraph <- thaw graph
        reversed <- Mutable.reverse mgraph >>= freeze
        forM_ (Immutable.bfs reversed terminals) $ \node -> do
          cnode <- dataFlowEquation mgraph node.index
          Mutable.updateNode mgraph node.index cnode
        freeze mgraph

dataFlowEquation :: (Ord var, PrimMonad m, MonadThrow m) => ControlFlowMutableGraph var val (PrimState m) -> NodeIndex -> m (ControlFlowNode var val)
dataFlowEquation mgraph index = do
  node <- Mutable.getNodeByIndex mgraph index
  successors <- mapM (Mutable.getNodeByIndex mgraph . getField @"target") node.outEdges
  let input = node.val.usedVariables `Set.union` (node.val.liveOutVariables Set.\\ node.val.definedVariables)
      output = Set.unions $ (\node -> node.val.liveInVariables) <$> successors
  pure node.val {liveInVariables = input, liveOutVariables = output}
