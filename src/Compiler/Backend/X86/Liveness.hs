module Compiler.Backend.X86.Liveness where

import Compiler.Backend.X86.Arch (Label)
import RIO

data ControlFlow var val
  = Instruction {src :: [var], dst :: [var], val :: val}
  | Jump {jumps :: [Label], val :: val}
  | CJump {jumps :: [Label], val :: val}
  | Label {label :: Label, val :: val}
  deriving (Show, Eq)
