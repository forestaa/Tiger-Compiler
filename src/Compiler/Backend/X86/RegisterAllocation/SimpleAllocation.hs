module Compiler.Backend.X86.RegisterAllocation.SimpleAllocation (SimpleAllocation) where

import Compiler.Backend.X86.Arch (Assembly (..), Register (..), callerSaveRegisters, replaceRegister)
import Compiler.Backend.X86.Frame (Access (..), Frame (..), FrameEff, ProcedureX86 (..), allTempRegisters, allocateLocalEff, allocateNonEscapedLocalEff, getAllocatedRegisters, modifyFrameEff, runFrameEff)
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), setDestinations, setSources)
import Compiler.Backend.X86.RegisterAllocation qualified as R (RegisterAllocation (..))
import Compiler.Backend.X86.RegisterAllocation.InterferenceGraph (InterferenceGraph, InterferenceMutableGraph, buildInterfereceGraph, thaw)
import Compiler.Backend.X86.RegisterAllocation.RegisterAllocator (Allocation, AvailableColors, RegisterAllocator (..), allocateEff, getColor, runRegisterAllocatorState)
import Compiler.Intermediate.Frame qualified as F (fp)
import Compiler.Intermediate.Unique qualified as U
import Compiler.Utils.Graph.Base (Node (..))
import Compiler.Utils.Graph.Mutable qualified as Mutable (MutableGraph (..))
import Data.Extensible (Lookup, type (>:))
import Data.Extensible.Effect (Eff, castEff)
import Data.Vector qualified as V
import GHC.Records (HasField (..))
import RIO
import RIO.List qualified as List
import RIO.Partial (fromJust)
import RIO.Set qualified as Set
import RIO.State (State)

data SimpleAllocation

instance R.RegisterAllocation SimpleAllocation where
  allocateRegisters = allocateRegisters

allocateRegisters :: Lookup xs "temp" U.UniqueEff => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProcedureX86 [Assembly Register])
allocateRegisters procedure = case simpleColoring callerSaveRegisters procedure of
  Spilled spilled -> do
    procedure <- foldM startOver procedure spilled
    allocateRegisters procedure
  Colored allocation ->
    let body = getField @"val" <$> procedure.body
        allocatedBody = replaceRegister (fromJust . getColor allocation) <$> body
     in pure Procedure {body = allocatedBody, frame = procedure.frame}

simpleColoring :: AvailableColors -> ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> ColoringResult
simpleColoring colors procedure =
  let graph = buildInterfereceGraph (allTempRegisters `Set.union` Set.fromList (getAllocatedRegisters procedure.frame)) procedure.body
      stacks = simplifyAndSpill colors graph
   in select colors graph stacks

type SimplfyStack = [U.Temp]

data ColoringResult = Spilled [U.Temp] | Colored Allocation

simplifyAndSpill :: AvailableColors -> InterferenceGraph U.Temp -> SimplfyStack
simplifyAndSpill colors graph = runST $ simplifyAndSpillLoop [] =<< thaw graph
  where
    simplifyAndSpillLoop :: (PrimMonad m, MonadThrow m) => SimplfyStack -> InterferenceMutableGraph U.Temp (PrimState m) -> m SimplfyStack
    simplifyAndSpillLoop stack graph = do
      nodes <- Mutable.getAllNodes graph
      let node = V.minimumBy (comparing (\n -> length n.outEdges)) nodes
      if
          | V.null nodes -> pure stack
          | length node.outEdges < length colors -> do
              -- colored
              Mutable.removeNode graph node
              simplifyAndSpillLoop (node.val : stack) graph
          | otherwise -> do
              -- possibly spilled
              let node = V.maximumBy (comparing (\n -> length n.outEdges)) nodes
              Mutable.removeNode graph node
              simplifyAndSpillLoop (node.val : stack) graph

select :: AvailableColors -> InterferenceGraph U.Temp -> SimplfyStack -> ColoringResult
select colors graph stack =
  let (missed, allocator) = runRegisterAllocatorState graph colors $ selectLoop stack
   in if List.null missed
        then Colored allocator.allocation
        else Spilled missed
  where
    selectLoop :: SimplfyStack -> State RegisterAllocator SimplfyStack
    selectLoop = filterM (fmap not . allocateEff)

startOver :: Lookup xs "temp" U.UniqueEff => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> U.Temp -> Eff xs (ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)])
startOver procedure spilledTemp = do
  (body, frame) <- castEff . flip runFrameEff procedure.frame $ do
    modifyFrameEff $ \frame -> frame {parameters = fmap (spillOutAccess spilledTemp) frame.parameters, localVariables = fmap (spillOutAccess spilledTemp) frame.localVariables}
    access <- allocateLocalEff True
    concat <$> mapM (insertSpillAssembly spilledTemp access) procedure.body :: Eff '["frame" >: FrameEff, "temp" >: U.UniqueEff] [L.ControlFlow U.Temp (Assembly U.Temp)]
  pure procedure {frame = frame, body = body}
  where
    spillOutAccess :: U.Temp -> Access -> Access
    spillOutAccess spilled (InRegister temp) | temp == spilled = SpilledOut
    spillOutAccess _ access = access
    insertSpillAssembly :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => U.Temp -> Access -> L.ControlFlow U.Temp (Assembly U.Temp) -> Eff xs [L.ControlFlow U.Temp (Assembly U.Temp)]
    insertSpillAssembly spilledTemp (InFrame memory) flow = do
      temp <- allocateNonEscapedLocalEff
      let newSrc = fmap (\register -> if register == spilledTemp then temp else register) flow.sources
          newDst = fmap (\register -> if register == spilledTemp then temp else register) flow.destinations
          newVal = fmap (\register -> if register == spilledTemp then temp else register) flow.val
          newFlow = L.setSources newSrc . L.setDestinations newDst $ flow {L.val = newVal}
      pure $
        [L.Instruction {src = [], dst = [temp], val = MovLoadIndirect memory (F.fp @Frame) temp} | spilledTemp `elem` flow.sources]
          ++ [newFlow]
          ++ [L.Instruction {src = [temp], dst = [], val = MovStoreIndirect temp memory (F.fp @Frame)} | spilledTemp `elem` flow.destinations]
    insertSpillAssembly _ _ _ = undefined
