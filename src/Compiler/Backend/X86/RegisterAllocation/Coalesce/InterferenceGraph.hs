module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph
  ( buildInterfereceGraph,
  )
where

import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), ControlFlowGraph, ControlFlowNode (..), newControlFlowGraph, solveDataFlowEquation)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base (InterferenceGraphEdgeLabel (..), Move (destination, source), addMove, newMove)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable (InterferenceGraph)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable qualified as Mutable (InterferenceMutableGraph, addEdge, addNode, empty, freeze, getEdges, getNode, updateNode)
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
    forM_ (getField @"val" <$> Immutable.getAllNodes cfGraph) $ addInterferenceGraphEdges graph
    Mutable.freeze graph
  where
    addInterferenceGraphEdges :: (PrimMonad m, Ord var, MonadThrow m) => Mutable.InterferenceMutableGraph var (PrimState m) -> L.ControlFlowNode var val -> m ()
    addInterferenceGraphEdges graph cnode
      | cnode.isMove =
          let notUsedOutput = cnode.liveOutVariables Set.\\ cnode.usedVariables Set.\\ cnode.definedVariables
              moves = newMove <$> Set.toList cnode.usedVariables <*> Set.toList cnode.definedVariables
           in do
                forM_ moves $ \move -> do
                  source <- Mutable.getNode graph move.source
                  Mutable.updateNode graph source.index $ addMove move source.val
                  target <- Mutable.getNode graph move.destination
                  Mutable.updateNode graph target.index $ addMove move target.val
                sequence_ $ addEdge <$> Set.toList cnode.definedVariables <*> Set.toList notUsedOutput
      | otherwise =
          let notDefinedOutput = cnode.liveOutVariables Set.\\ cnode.definedVariables
           in sequence_ $ addEdge <$> Set.toList cnode.definedVariables <*> Set.toList notDefinedOutput
      where
        addEdge src tgt =
          whenM (Vec.null <$> Mutable.getEdges graph src tgt) . void $
            Mutable.addEdge graph src tgt InterferenceGraphEdgeLabel

buildInterfereceGraph :: (Ord var) => Set.Set var -> [L.ControlFlow var val] -> InterferenceGraph var
buildInterfereceGraph vars = newInterferenceGraph vars . L.solveDataFlowEquation . L.newControlFlowGraph
