module Compiler.Backend.X86.RegisterAllocation (allocateRegisters) where

import Compiler.Backend.X86.Arch (Assembly (..), Register (..), allTemporaryRegisters, replaceRegister)
import Compiler.Backend.X86.Frame (Access (..), Frame (..), ProcedureX86 (..), allocateLocal, getAllocatedRegisters)
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..))
import Compiler.Backend.X86.RegisterAllocation.InterferenceGraph (InterferenceGraph, InterferenceMutableGraph, buildInterfereceGraph, thaw)
import Compiler.Intermediate.Frame qualified as F (fp)
import Compiler.Intermediate.Unique qualified as U
import Compiler.Utils.Graph.Base (Node (..))
import Compiler.Utils.Graph.Immutable qualified as Immutable (ImmutableGraph (..), getNode, getNodeByIndex)
import Compiler.Utils.Graph.Mutable qualified as Mutable (MutableGraph (..))
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)
import Data.Vector qualified as V
import GHC.Records (HasField (..))
import RIO
import RIO.List qualified as List
import RIO.Map qualified as Map (Map, empty, insert, (!?))
import RIO.Partial (fromJust)
import RIO.Set qualified as Set
import RIO.State (MonadState, State, runState, state)

allocateRegisters :: Lookup xs "temp" U.UniqueEff => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProcedureX86 [Assembly Register])
allocateRegisters procedure = case simpleColoring allTemporaryRegisters procedure of
  Spilled spilled -> do
    procedure <- foldM startOver procedure spilled
    allocateRegisters procedure
  Colored allocation ->
    let body = (\flow -> flow.val) <$> procedure.body
        allocatedBody = replaceRegister (fromJust . getColor allocation) <$> body
     in pure Procedure {body = allocatedBody, frame = procedure.frame}

simpleColoring :: [Register] -> ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> ColoringResult
simpleColoring colors procedure =
  let graph = buildInterfereceGraph (Set.fromList (getAllocatedRegisters procedure.frame)) procedure.body
      stacks = simplifyAndSpill colors graph
   in select colors graph stacks

data ColoringResult = Spilled [U.Temp] | Colored Allocation

simplifyAndSpill :: [Register] -> InterferenceGraph U.Temp -> [U.Temp]
simplifyAndSpill colors graph = runST $ simplifyAndSpillLoop =<< thaw graph
  where
    simplifyAndSpillLoop :: (PrimMonad m, MonadThrow m) => InterferenceMutableGraph U.Temp (PrimState m) -> m [U.Temp]
    simplifyAndSpillLoop graph = do
      node <- V.minimumBy (comparing (\n -> length n.outEdges)) <$> Mutable.getAllNodes graph
      if length node.outEdges < length colors
        then do
          -- colored
          Mutable.removeNode graph node
          (:) node.val <$> simplifyAndSpillLoop graph
        else do
          -- possibly spilled
          node <- V.maximumBy (comparing (\n -> length n.outEdges)) <$> Mutable.getAllNodes graph
          Mutable.removeNode graph node
          (:) node.val <$> simplifyAndSpillLoop graph

select :: [Register] -> InterferenceGraph U.Temp -> [U.Temp] -> ColoringResult
select colors graph stack =
  let (missed, allocator) = runRegisterAllocatorState graph colors $ selectLoop stack
   in if List.null missed
        then Colored allocator.allocation
        else Spilled missed
  where
    selectLoop :: [U.Temp] -> State RegisterAllocator [U.Temp]
    selectLoop = filterM (fmap not . allocateEff)

startOver :: Lookup xs "temp" U.UniqueEff => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> U.Temp -> Eff xs (ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)])
startOver procedure spilledTemp = do
  let frame = procedure.frame {parameters = fmap (spilledOutAccess spilledTemp) procedure.frame.parameters, localVariables = fmap (spillOutAccess spilledTemp) procedure.frame.localVariables}
  (frame, access) <- allocateLocal frame True -- TODO: allocateLocalEff
  body <- concat <$> mapM (insertSpillAssembly spilledTemp access) procedure.body
  pure procedure {frame = frame, body = body}
  where
    spillOutAccess :: U.Temp -> Access -> Access
    spillOutAccess spilled (InRegister temp) | temp == spilled = SpilledOut
    spilledOutAccess _ access = access
    insertSpillAssembly :: Lookup xs "temp" U.UniqueEff => U.Temp -> Access -> L.ControlFlow U.Temp (Assembly U.Temp) -> Eff xs [L.ControlFlow U.Temp (Assembly U.Temp)]
    insertSpillAssembly spilledTemp (InFrame memory) flow
      | spilledTemp `elem` flow.sources = do
          temp <- U.newTemp
          let newVal = fmap (\register -> if register == spilledTemp then temp else register) flow.val
          pure
            [ L.Instruction {src = [], dst = [temp], val = MovLoadIndirect memory (F.fp @Frame) temp},
              flow {L.val = newVal}
            ]
      | spilledTemp `elem` flow.destinations = do
          temp <- U.newTemp
          let newVal = fmap (\register -> if register == spilledTemp then temp else register) flow.val
          pure
            [ flow {L.val = newVal},
              L.Instruction {src = [spilledTemp], dst = [], val = MovStoreIndirect temp memory (F.fp @Frame)}
            ]
      | otherwise = pure [flow]
    insertSpillAssembly _ _ flow = pure [flow]

newtype Allocation = Allocation (Map.Map U.Temp Register)

newAllocation :: Allocation
newAllocation = Allocation Map.empty

getColor :: Allocation -> U.Temp -> Maybe Register
getColor (Allocation map) temp = map Map.!? temp

getColors :: Allocation -> [U.Temp] -> [Register]
getColors allocation = mapMaybe (getColor allocation)

putColor :: Allocation -> U.Temp -> Register -> Allocation
putColor (Allocation map) temp register = Allocation $ Map.insert temp register map

data RegisterAllocator = RegisterAllocator {graph :: InterferenceGraph U.Temp, availableColors :: [Register], allocation :: Allocation}

newRegisterAllocator :: InterferenceGraph U.Temp -> [Register] -> RegisterAllocator
newRegisterAllocator graph availableColors = RegisterAllocator {graph, availableColors, allocation = newAllocation}

allocate :: RegisterAllocator -> U.Temp -> (Bool, RegisterAllocator)
allocate allocator temp =
  let node = Immutable.getNode allocator.graph temp
      neiborhoods = V.toList $ fmap (getField @"val" . Immutable.getNodeByIndex allocator.graph) node.outIndexes
      allocatableColors = allocator.availableColors List.\\ getColors allocator.allocation neiborhoods
   in case List.headMaybe allocatableColors of
        Nothing -> (False, allocator)
        Just color -> (True, allocator {allocation = putColor allocator.allocation temp color})

allocateEff :: (MonadState RegisterAllocator m) => U.Temp -> m Bool
allocateEff temp = state (`allocate` temp)

runRegisterAllocatorState :: InterferenceGraph U.Temp -> [Register] -> State RegisterAllocator a -> (a, RegisterAllocator)
runRegisterAllocatorState graph availableColors m = runState m $ newRegisterAllocator graph availableColors
