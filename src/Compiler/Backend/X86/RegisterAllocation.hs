module Compiler.Backend.X86.RegisterAllocation (RegisterAllocation (..)) where

import Compiler.Backend.X86.Arch (Assembly (..), Register (..))
import Compiler.Backend.X86.Frame (ProcedureX86 (..))
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..))
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)

class RegisterAllocation r where
  allocateRegisters :: forall xs. (Lookup xs "temp" U.UniqueEff) => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProcedureX86 [Assembly Register])
