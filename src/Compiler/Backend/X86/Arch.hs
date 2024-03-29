module Compiler.Backend.X86.Arch where

import Compiler.Intermediate.Unique qualified as U
import RIO

newtype Memory = Memory Int deriving (Show, Eq)

newtype Label = Label' Text deriving (Show, Eq, Ord, Display)

fromUniqueLabel :: U.Label -> Label
fromUniqueLabel = Label' . textDisplay

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
  deriving (Show, Eq, Ord)

allTemporaryRegisters :: [Register]
allTemporaryRegisters = [RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11, R12, R13, R14, R15]

callerSaveRegisters :: [Register]
callerSaveRegisters = [RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11]

calleeSaveRegisters :: [Register]
calleeSaveRegisters = [RBX, RSP, RBP, R12, R13, R14, R15]

data Type = Function | Object deriving (Show, Eq)

data Assembly register
  = MovImmediate Int register
  | MovImmediateLabel Label register
  | MovRegister register register
  | MovLoad Memory register
  | MovLoadIndirect Int register register
  | MovLoadDisplacement Int register register Int register
  | MovStore register Memory
  | MovStoreIndirect register Int register
  | Lea Label register register
  | AddImmediate Int register
  | AddRegister register register
  | SubImmediate Int register
  | SubRegister register register
  | MulImmediate Int register register
  | MulRegister register register
  | DivRegister register
  | Cqo
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
  | Leave
  | Ret
  | PushImmediate Int
  | PushRegister register
  | Pop register
  | Label Label
  | Global Label
  | Data
  | Text
  | Align Int
  | Type Label Type
  | Size Label Int
  | String Text
  | Zero Int
  | Quad Int
  deriving (Show, Eq, Functor)

replaceRegister :: (register1 -> register2) -> Assembly register1 -> Assembly register2
replaceRegister = fmap
