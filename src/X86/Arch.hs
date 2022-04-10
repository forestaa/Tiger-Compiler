module X86.Arch where

import RIO
import Unique qualified as U

newtype Memory = Memory Int deriving (Show, Eq)

newtype Label = Label' String deriving (Show, Eq)

fromUniqueLabel :: U.Label -> Label
fromUniqueLabel = Label' . show

data Register
  = RAX
  | RDI
  | RSI
  | RDX
  | RCX
  | RBP
  | RSP
  | RBX
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | RIP
  | EFLAGS
  deriving (Show, Eq)

data Assembly register
  = MovImmediate Int register
  | MovImmediateLabel Label register
  | MovRegister register register
  | MovLoad Memory register
  | MovLoadIndirect Int register register
  | MovLoadDisplacement Int register register Integer register
  | MovStore register Memory
  | MovStoreIndirect register Int register
  | AddImmediate Int register
  | AddRegister register register
  | SubImmediate Int register
  | SubRegister register register
  | CmpImmediate register Int
  | CmpRegister register register
  | Jump Label
  | JumpIfEqual Label
  | JumpIfNotEqual Label
  | JumpIfLessThan Label
  | JumpIfGreaterThan Label
  | JumpIfEqualOrLessThan Label
  | JumpIfEqualOrGreaterThan Label
  | Call Label
  | Ret
  | PushImmediate Int
  | PushRegister register
  | Pop register
  | Label Label
  deriving (Show, Eq)
