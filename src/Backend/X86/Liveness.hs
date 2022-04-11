module Backend.X86.Liveness where

import Backend.X86.Arch (Label)
import RIO

data ControlFlow var val
  = Instruction {src :: [var], dst :: [var], val :: val}
  | Jump {jumps :: [Label], val :: val}
  | CJump {jumps :: [Label], val :: val}
  | Label {label :: Label, val :: val}
  deriving (Show, Eq)
