module X86.Liveness where

import RIO
import X86.Arch (Label)

data ControlFlow var val
  = Instruction {src :: [var], dst :: [var], val :: val}
  | Jump {jumps :: [Label], val :: val}
  | CJump {jumps :: [Label], val :: val}
  | Label {label :: Label, val :: val}
  deriving (Show, Eq)
