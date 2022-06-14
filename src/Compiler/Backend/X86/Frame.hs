module Compiler.Backend.X86.Frame where

import Compiler.Backend.X86.Arch
import Compiler.Intermediate.Frame qualified as Frame
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)
import RIO hiding (exp)
import RIO.List.Partial (last)
import RIO.Map qualified as Map
import RIO.Map.Partial

data Frame = Frame {name :: U.Label, parameters :: [Access], localVariables :: [Access], head :: Int}

data Access = InRegister U.Temp | InFrame Int

isInRegister :: Access -> Bool
isInRegister (InRegister _) = True
isInRegister _ = False

isInFrame :: Access -> Bool
isInFrame (InFrame _) = True
isInFrame _ = False

emptyFrame :: U.Label -> Frame
emptyFrame label = Frame label [] [] 0

newFrame :: Lookup xs "temp" U.UniqueEff => U.Label -> [Bool] -> Eff xs Frame
newFrame label parameters = foldM allocateParameter (emptyFrame label) parameters

allocateParameter :: Lookup xs "temp" U.UniqueEff => Frame -> Bool -> Eff xs Frame
allocateParameter frame True =
  let access = InFrame frame.head
   in pure frame {parameters = frame.parameters ++ [access], head = frame.head - wordSize}
allocateParameter frame False = do
  t <- U.newTemp
  let access = InRegister t
  pure frame {parameters = frame.parameters ++ [access]}

allocateLocal :: Lookup xs "temp" U.UniqueEff => Frame -> Bool -> Eff xs Frame
allocateLocal frame True = do
  let access = InFrame frame.head
   in pure frame {localVariables = frame.localVariables ++ [access], head = frame.head - wordSize}
allocateLocal frame False = do
  t <- U.newTemp
  let access = InRegister t
  pure frame {localVariables = frame.localVariables ++ [access]}

numberOfFrameAllocatedVariables :: Frame -> Int
numberOfFrameAllocatedVariables frame = length $ filter isInFrame frame.localVariables

exp :: Access -> IR.Exp -> IR.Exp
exp (InRegister t) _ = IR.Temp t
exp (InFrame offset) base = IR.Mem (IR.BinOp IR.Plus base (IR.Const offset))

bp :: U.Temp
bp = registerTempMap ! RBP

rv :: U.Temp
rv = registerTempMap ! RAX

rip :: U.Temp
rip = registerTempMap ! RIP

rax :: U.Temp
rax = registerTempMap ! RAX

rsp :: U.Temp
rsp = registerTempMap ! RSP

rbp :: U.Temp
rbp = registerTempMap ! RBP

wordSize :: Int
wordSize = 8

externalCall :: Lookup xs "label" U.UniqueEff => String -> [IR.Exp] -> Eff xs IR.Exp
externalCall f parameters = do
  label <- U.namedLabel f -- TODO: label must be fixed and must not depend on unique
  pure $ IR.Call (IR.Name label) parameters

parameterReceiving :: Frame -> IR.Stm
parameterReceiving frame = foldl' IR.Seq IR.noop $ zipWith parameterReceivingStm frame.parameters parameterPassingAccesses
  where
    parameterReceivingStm to (InRegister temp) = IR.Move (exp to (IR.Temp bp)) (IR.Temp temp)
    parameterReceivingStm to (InFrame offset) = IR.Move (exp to (IR.Temp bp)) (IR.Mem (IR.BinOp IR.Plus (IR.Temp bp) (IR.Const offset)))

parameterPassingAccesses :: [Access]
parameterPassingAccesses = (InRegister <$> parameterTempRegisters) ++ [InFrame (wordSize * i) | i <- [2 ..]]

parameterRegisters :: [Register]
parameterRegisters = [RDI, RSI, RDX, RCX, R8, R9]

parameterTempRegisters :: [U.Temp]
parameterTempRegisters = (!) registerTempMap <$> parameterRegisters

prologue :: Frame -> [Assembly U.Temp]
prologue frame
  | numberOfFrameAllocatedVariables frame == 0 =
      [ PushRegister rbp,
        MovRegister rsp rbp
      ]
  | otherwise =
      [ PushRegister rbp,
        MovRegister rsp rbp,
        SubImmediate (wordSize * numberOfFrameAllocatedVariables frame) rsp
      ]

epilogue :: [Assembly U.Temp]
epilogue =
  [ Leave,
    Ret
  ]

registerTempMap :: Map Register U.Temp
registerTempMap =
  Map.fromList
    [ (RAX, U.newStringTemp "RAX"),
      (RDI, U.newStringTemp "RDI"),
      (RSI, U.newStringTemp "RSI"),
      (RDX, U.newStringTemp "RDX"),
      (RCX, U.newStringTemp "RCX"),
      (RBP, U.newStringTemp "RBP"),
      (RSP, U.newStringTemp "RSP"),
      (RBX, U.newStringTemp "RBX"),
      (R8, U.newStringTemp "R8"),
      (R9, U.newStringTemp "R9"),
      (R10, U.newStringTemp "R10"),
      (R11, U.newStringTemp "R11"),
      (R12, U.newStringTemp "R12"),
      (R13, U.newStringTemp "R13"),
      (R14, U.newStringTemp "R14"),
      (R15, U.newStringTemp "R15"),
      (RIP, U.newStringTemp "RIP"),
      (EFLAGS, U.newStringTemp "EFLAGS")
    ]

instance Frame.Frame Frame where
  type Access Frame = Access
  newFrame = newFrame
  name frame = frame.name
  formals frame = frame.parameters
  allocLocal frame escape = (\frame -> (frame, last frame.localVariables)) <$> allocateLocal frame escape
  fp = bp
  rv = rv
  exp = exp
  wordSize = wordSize
  externalCall = externalCall
  procEntryExit1 frame stm = parameterReceiving frame `IR.Seq` stm
