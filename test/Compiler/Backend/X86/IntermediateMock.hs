module Compiler.Backend.X86.IntermediateMock where

import Compiler.Intermediate (Intermediate (..))
import Compiler.Intermediate.Canonical (linearize)

data IntermediateMock = IntermediateMock

instance Intermediate IntermediateMock where
  processIntermediate = linearize
