module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph
  ( newInterferenceGraph,
    buildInterfereceGraph,
  )
where

import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), ControlFlowGraph, ControlFlowNode (..), newControlFlowGraph, solveDataFlowEquation)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base qualified as B (InterferenceGraphEdgeLabel (..), addMove, constrainMove, newMove)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable (InterferenceGraph)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable qualified as Mutable (InterferenceMutableGraph, addEdge, addNode, empty, freeze, getEdges, getEdgesByIndex, getNode, updateNode)
import Compiler.Utils.Graph.Base (Node (..))
import Compiler.Utils.Graph.Immutable qualified as Immutable (ImmutableGraph (..))
import GHC.Records (HasField (getField))
import RIO
import RIO.Set qualified as Set
import RIO.Vector qualified as Vec

newInterferenceGraph :: Ord var => Set.Set var -> L.ControlFlowGraph var val -> InterferenceGraph var
newInterferenceGraph vars cfGraph =
  runST $ do
    graph <- Mutable.empty
    forM_ vars $ Mutable.addNode graph
    let allNodes = getField @"val" <$> Immutable.getAllNodes cfGraph
    forM_ allNodes $ addInterferenceGraphEdges graph
    forM_ allNodes $ addMove graph
    Mutable.freeze graph
  where
    addInterferenceGraphEdges :: (PrimMonad m, Ord var, MonadThrow m) => Mutable.InterferenceMutableGraph var (PrimState m) -> L.ControlFlowNode var val -> m ()
    addInterferenceGraphEdges graph cnode
      | cnode.isMove = do
          let notUsedOutputs = cnode.liveOutVariables Set.\\ cnode.usedVariables Set.\\ cnode.definedVariables
          forM_ cnode.definedVariables $ \definedVariable ->
            forM_ notUsedOutputs $ \notUsedOutput ->
              addEdge definedVariable notUsedOutput
      | otherwise = do
          let notDefinedOutputs = cnode.liveOutVariables Set.\\ cnode.definedVariables
          forM_ cnode.definedVariables $ \definedVariable ->
            forM_ notDefinedOutputs $ \notDefinedOutput ->
              addEdge definedVariable notDefinedOutput
      where
        addEdge src tgt =
          whenM (Vec.null <$> Mutable.getEdges graph src tgt) . void $
            Mutable.addEdge graph src tgt B.InterferenceGraphEdgeLabel
    addMove :: (PrimMonad m, Ord var, MonadThrow m) => Mutable.InterferenceMutableGraph var (PrimState m) -> L.ControlFlowNode var val -> m ()
    addMove graph cnode
      | cnode.isMove = do
          srcs <- mapM (\src -> (src,) <$> Mutable.getNode graph src) $ Set.toList cnode.usedVariables
          dests <- mapM (\dest -> (dest,) <$> Mutable.getNode graph dest) $ Set.toList cnode.definedVariables
          forM_ srcs $ \(src, srcNode) ->
            forM_ dests $ \(dest, destNode) -> do
              edges <- Mutable.getEdgesByIndex graph srcNode.index destNode.index
              let move = B.newMove src dest
                  newSrcNode = if null edges then B.addMove move srcNode.val else B.constrainMove move srcNode.val
                  newDestNode = if null edges then B.addMove move destNode.val else B.constrainMove move destNode.val
              Mutable.updateNode graph srcNode.index newSrcNode
              Mutable.updateNode graph destNode.index newDestNode
      | otherwise = pure ()

buildInterfereceGraph :: (Ord var) => Set.Set var -> [L.ControlFlow var val] -> InterferenceGraph var
buildInterfereceGraph vars = newInterferenceGraph vars . L.solveDataFlowEquation . L.newControlFlowGraph
