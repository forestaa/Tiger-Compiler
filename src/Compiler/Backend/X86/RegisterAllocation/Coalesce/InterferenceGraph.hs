module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph
  ( newInterferenceGraph,
    buildInterfereceGraph,
  )
where

import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), ControlFlowGraph, ControlFlowNode (..), newControlFlowGraph, solveDataFlowEquation)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base qualified as B (newMove)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable (InterferenceGraph)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable qualified as Mutable (InterferenceMutableGraph, addEdge, addMove, addNode, constrainMove, empty, freeze, getEdge)
import Compiler.Utils.Graph.Base (Node (..))
import Compiler.Utils.Graph.Immutable qualified as Immutable (ImmutableGraph (..))
import RIO
import RIO.Set qualified as Set

newInterferenceGraph :: (Ord var) => Set.Set var -> L.ControlFlowGraph var val -> InterferenceGraph var
newInterferenceGraph vars cfGraph =
  runST $ do
    graph <- Mutable.empty
    forM_ vars $ Mutable.addNode graph
    let allNodes = (.val) <$> Immutable.getAllNodes cfGraph
    forM_ allNodes $ addInterferenceGraphEdges vars graph
    forM_ allNodes $ addMove vars graph
    Mutable.freeze graph
  where
    addInterferenceGraphEdges :: (PrimMonad m, Ord var, MonadThrow m) => Set.Set var -> Mutable.InterferenceMutableGraph var (PrimState m) -> L.ControlFlowNode var val -> m ()
    addInterferenceGraphEdges vars graph cnode
      | cnode.isMove = do
          let notUsedOutputs = cnode.liveOutVariables Set.\\ cnode.usedVariables Set.\\ cnode.definedVariables
          forM_ (cnode.definedVariables `Set.intersection` vars) $ \definedVariable ->
            forM_ (notUsedOutputs `Set.intersection` vars) $ \notUsedOutput ->
              Mutable.addEdge graph definedVariable notUsedOutput
      | otherwise = do
          let notDefinedOutputs = cnode.liveOutVariables Set.\\ cnode.definedVariables
          forM_ (cnode.definedVariables `Set.intersection` vars) $ \definedVariable ->
            forM_ (notDefinedOutputs `Set.intersection` vars) $ \notDefinedOutput ->
              Mutable.addEdge graph definedVariable notDefinedOutput
    addMove :: (PrimMonad m, Ord var, MonadThrow m) => Set.Set var -> Mutable.InterferenceMutableGraph var (PrimState m) -> L.ControlFlowNode var val -> m ()
    addMove vars graph cnode
      | cnode.isMove = do
          forM_ (cnode.usedVariables `Set.intersection` vars) $ \src ->
            forM_ (cnode.definedVariables `Set.intersection` vars) $ \dest -> do
              edge <- Mutable.getEdge graph src dest
              let move = B.newMove src dest
              if isNothing edge
                then Mutable.addMove graph move
                else Mutable.constrainMove graph move
      | otherwise = pure ()

buildInterfereceGraph :: (Ord var) => Set.Set var -> [L.ControlFlow var val] -> InterferenceGraph var
buildInterfereceGraph vars = newInterferenceGraph vars . L.solveDataFlowEquation . L.newControlFlowGraph
