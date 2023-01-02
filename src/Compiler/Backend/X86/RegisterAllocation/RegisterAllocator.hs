module Compiler.Backend.X86.RegisterAllocation.RegisterAllocator
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
import Compiler.Backend.X86.RegisterAllocation.InterferenceGraph (InterferenceGraph)
import Compiler.Intermediate.Unique qualified as U
import Compiler.Utils.Graph.Base (Node (..))
import Compiler.Utils.Graph.Immutable qualified as Immutable (ImmutableGraph (..), getNode, getNodeByIndex)
import Data.Vector qualified as V
import GHC.Records (HasField (..))
import RIO
import RIO.List qualified as List
import RIO.Map qualified as Map (Map, insert, (!?))
import RIO.State (MonadState, State, runState, state)

newtype Allocation = Allocation (Map.Map U.Temp Register) deriving (Show)

newAllocation :: Allocation
newAllocation = Allocation inverseRegisterTempMap

getColor :: Allocation -> U.Temp -> Maybe Register
getColor (Allocation map) temp = map Map.!? temp

getColors :: Allocation -> [U.Temp] -> [Register]
getColors allocation = mapMaybe (getColor allocation)

-- TODO: raise an exception when the register is already allocated
putColor :: Allocation -> U.Temp -> Register -> Allocation
putColor (Allocation map) temp register = Allocation $ Map.insert temp register map

type AvailableColors = [Register]

data RegisterAllocator = RegisterAllocator {graph :: InterferenceGraph U.Temp, availableColors :: AvailableColors, allocation :: Allocation}

newRegisterAllocator :: InterferenceGraph U.Temp -> AvailableColors -> RegisterAllocator
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

runRegisterAllocatorState :: InterferenceGraph U.Temp -> AvailableColors -> State RegisterAllocator a -> (a, RegisterAllocator)
runRegisterAllocatorState graph availableColors m = runState m $ newRegisterAllocator graph availableColors
