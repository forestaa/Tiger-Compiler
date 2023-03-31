module Compiler.Backend.X86.RegisterAllocation.Coalesce.RegisterAllocator
  ( Allocation,
    getColor,
    AvailableColors,
    RegisterAllocator (allocation),
    runRegisterAllocatorState,
    allocateEff,
  )
where

import Compiler.Backend.X86.Arch (Register (..))
import Compiler.Backend.X86.Frame (inverseRegisterTempMap)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base (InterferenceGraphNode (vars))
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable qualified as Immutable (InterferenceGraph, getOutNeiborhoods)
import Compiler.Intermediate.Unique qualified as U
import Compiler.Utils.Graph.Base (Node (..))
import GHC.Records (HasField (..))
import RIO
import RIO.List qualified as List
import RIO.Map qualified as Map (Map, insert, (!?))
import RIO.Set qualified as Set (Set, map, toList, unions)
import RIO.State (MonadState, State, runState, state)

newtype Allocation = Allocation (Map.Map U.Temp Register) deriving (Show)

newAllocation :: Allocation
newAllocation = Allocation inverseRegisterTempMap

isAllocated :: Allocation -> U.Temp -> Bool
isAllocated = (.) isJust . getColor

getColor :: Allocation -> U.Temp -> Maybe Register
getColor (Allocation map) temp = map Map.!? temp

getColors :: Allocation -> [U.Temp] -> [Register]
getColors allocation = mapMaybe (getColor allocation)

-- TODO: raise an exception when the register is already allocated
putColor :: Allocation -> U.Temp -> Register -> Allocation
putColor (Allocation map) temp register = Allocation $ Map.insert temp register map

putColors :: Allocation -> Set.Set U.Temp -> Register -> Allocation
putColors allocation temps register = foldr (\temp allocation -> putColor allocation temp register) allocation temps

type AvailableColors = [Register]

data RegisterAllocator = RegisterAllocator {graph :: Immutable.InterferenceGraph U.Temp, availableColors :: AvailableColors, allocation :: Allocation}

newRegisterAllocator :: Immutable.InterferenceGraph U.Temp -> AvailableColors -> RegisterAllocator
newRegisterAllocator graph availableColors = RegisterAllocator {graph, availableColors, allocation = newAllocation}

allocate :: RegisterAllocator -> Set.Set U.Temp -> (Bool, RegisterAllocator)
allocate allocator temp =
  let precolors = catMaybes . Set.toList $ Set.map (getColor allocator.allocation) temp
      neiborhoods = Set.toList $ foldMap (Set.unions . fmap ((.val.vars)) . Immutable.getOutNeiborhoods allocator.graph) temp
      allocatableColors = allocator.availableColors List.\\ getColors allocator.allocation neiborhoods
   in case (precolors, List.headMaybe allocatableColors) of
        ([color], _) -> (True, allocator {allocation = putColors allocator.allocation temp color})
        ([], Nothing) -> (False, allocator)
        ([], Just color) -> (True, allocator {allocation = putColors allocator.allocation temp color})
        (_, _) -> error "different colors are assigned to coalesced nodes"

allocateEff :: (MonadState RegisterAllocator m) => Set.Set U.Temp -> m Bool
allocateEff temp = state (`allocate` temp)

runRegisterAllocatorState :: Immutable.InterferenceGraph U.Temp -> AvailableColors -> State RegisterAllocator a -> (a, RegisterAllocator)
runRegisterAllocatorState graph availableColors m = runState m $ newRegisterAllocator graph availableColors
