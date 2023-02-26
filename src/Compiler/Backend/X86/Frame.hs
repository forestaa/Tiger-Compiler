module Compiler.Backend.X86.Frame where

import Compiler.Backend.X86.Arch
import Compiler.Intermediate.Frame qualified as Frame
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible (Lookup, type (>:))
import Data.Extensible.Effect (Eff, State, getEff, modifyEff, putEff, runStateEff)
import Data.Maybe (fromJust)
import GHC.Records (HasField (..))
import RIO hiding (exp)
import RIO.List qualified as List (findIndex, splitAt)
import RIO.Map qualified as Map
import RIO.Map.Partial qualified as Map
import RIO.Set qualified as Set

data Frame = Frame {name :: U.Label, parameters :: [Access], localVariables :: [Access], head :: Int} deriving (Show)

data Access = InRegister U.Temp | InFrame Int | SpilledOut deriving (Show)

isInRegister :: Access -> Bool
isInRegister (InRegister _) = True
isInRegister _ = False

isInFrame :: Access -> Bool
isInFrame (InFrame _) = True
isInFrame _ = False

emptyFrame :: U.Label -> Frame
emptyFrame label = Frame label [] [] (-wordSize)

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

allocateLocal :: Lookup xs "temp" U.UniqueEff => Frame -> Bool -> Eff xs (Frame, Access)
allocateLocal frame True = do
  let access = InFrame frame.head
   in pure (frame {localVariables = frame.localVariables ++ [access], head = frame.head - wordSize}, access)
allocateLocal frame False = do
  t <- U.newTemp
  let access = InRegister t
  pure (frame {localVariables = frame.localVariables ++ [access]}, access)

getAllocatedRegisters :: Frame -> [U.Temp]
getAllocatedRegisters frame = fmap (\case InRegister t -> t; _ -> undefined) . filter isInRegister $ frame.parameters ++ frame.localVariables

numberOfFrameAllocatedVariables :: Frame -> Int
numberOfFrameAllocatedVariables frame = length $ filter isInFrame frame.localVariables

exp :: Access -> IR.Exp -> IR.Exp
exp (InRegister t) _ = IR.Temp t
exp (InFrame offset) base = IR.Mem (IR.BinOp IR.Plus base (IR.Const offset))
exp SpilledOut _ = undefined

allTempRegisters :: Set.Set U.Temp
allTempRegisters = Set.fromList [rip, rax, rsp, rbp, rbx, rdi, rsi, rdx, rcx, r8, r9, r10, r11, r12, r13, r14, r15, eflags]

callerSaveTempRegisters :: [U.Temp]
callerSaveTempRegisters = (registerTempMap Map.!) <$> callerSaveRegisters

rip :: U.Temp
rip = U.newUniqueTextTemp "RIP"

rax :: U.Temp
rax = U.newUniqueTextTemp "RAX"

rsp :: U.Temp
rsp = U.newUniqueTextTemp "RSP"

rbp :: U.Temp
rbp = U.newUniqueTextTemp "RBP"

rbx :: U.Temp
rbx = U.newUniqueTextTemp "RBX"

rdi :: U.Temp
rdi = U.newUniqueTextTemp "RDI"

rsi :: U.Temp
rsi = U.newUniqueTextTemp "RSI"

rdx :: U.Temp
rdx = U.newUniqueTextTemp "RDX"

rcx :: U.Temp
rcx = U.newUniqueTextTemp "RCX"

r8 :: U.Temp
r8 = U.newUniqueTextTemp "R8"

r9 :: U.Temp
r9 = U.newUniqueTextTemp "R9"

r10 :: U.Temp
r10 = U.newUniqueTextTemp "R10"

r11 :: U.Temp
r11 = U.newUniqueTextTemp "R11"

r12 :: U.Temp
r12 = U.newUniqueTextTemp "R12"

r13 :: U.Temp
r13 = U.newUniqueTextTemp "R13"

r14 :: U.Temp
r14 = U.newUniqueTextTemp "R14"

r15 :: U.Temp
r15 = U.newUniqueTextTemp "R15"

eflags :: U.Temp
eflags = U.newUniqueTextTemp "EFLAGS"

wordSize :: Int
wordSize = 8

externalCall :: Lookup xs "label" U.UniqueEff => Text -> [IR.Exp] -> Eff xs IR.Exp
externalCall f parameters = do
  label <- U.namedLabel f -- TODO: label must be fixed and must not depend on unique
  pure $ IR.Call (IR.Name label) parameters

parameterReceiving :: Frame -> IR.Stm
parameterReceiving frame = foldl' IR.Seq IR.noop $ zipWith parameterReceivingStm frame.parameters parameterPassingAccesses
  where
    parameterReceivingStm to (InRegister temp) = IR.Move (exp to (IR.Temp rbp)) (IR.Temp temp)
    parameterReceivingStm to (InFrame offset) = IR.Move (exp to (IR.Temp rbp)) (IR.Mem (IR.BinOp IR.Plus (IR.Temp rbp) (IR.Const offset)))
    parameterReceivingStm _ SpilledOut = undefined

parameterPassingAccesses :: [Access]
parameterPassingAccesses = (InRegister <$> parameterTempRegisters) ++ [InFrame (wordSize * i) | i <- [2 ..]]

parameterTempRegisters :: [U.Temp]
parameterTempRegisters = [rdi, rsi, rdx, rcx, r8, r9]

-- NOTE: Normally PushRegister RBP is at the first of fragments, but for Tiger that is unnessary because RBP is passed as first argument of procedure
prologue :: Frame -> [Assembly Register]
prologue frame
  | numberOfFrameAllocatedVariables frame == 0 =
      [ PushRegister RBP,
        MovRegister RSP RBP
      ]
  | otherwise =
      [ PushRegister RBP,
        MovRegister RSP RBP,
        SubImmediate (wordSize * numberOfFrameAllocatedVariables frame) RSP
      ]

epilogue :: [Assembly Register]
epilogue =
  [ Leave,
    Ret
  ]

procEntryExit3 :: ProcedureX86 [Assembly Register] -> ProcedureX86 [Assembly Register]
procEntryExit3 procedure =
  let entryIndex = fromJust $ List.findIndex (\case Label label' -> label' == fromUniqueLabel (Frame.name procedure.frame); _ -> False) procedure.body
      (prefix, suffix) = List.splitAt (entryIndex + 1) procedure.body
      flows = concat [prefix, prologue procedure.frame, suffix, epilogue]
   in Procedure {body = flows, frame = procedure.frame}

registerTempMap :: Map Register U.Temp
registerTempMap =
  Map.fromList
    [ (RAX, rax),
      (RDI, rdi),
      (RSI, rsi),
      (RDX, rdx),
      (RCX, rcx),
      (RBP, rbp),
      (RSP, rsp),
      (RBX, rbx),
      (R8, r8),
      (R9, r9),
      (R10, r10),
      (R11, r11),
      (R12, r12),
      (R13, r13),
      (R14, r14),
      (R15, r15),
      (RIP, r10),
      (EFLAGS, eflags)
    ]

inverseRegisterTempMap :: Map U.Temp Register
inverseRegisterTempMap =
  Map.fromList
    [ (rax, RAX),
      (rdi, RDI),
      (rsi, RSI),
      (rdx, RDX),
      (rcx, RCX),
      (rbp, RBP),
      (rsp, RSP),
      (rbx, RBX),
      (r8, R8),
      (r9, R9),
      (r10, R10),
      (r11, R11),
      (r12, R12),
      (r13, R13),
      (r14, R14),
      (r15, R15),
      (rip, RIP),
      (eflags, EFLAGS)
    ]

data ProcedureX86 body = Procedure {body :: body, frame :: Frame} deriving (Show)

data StringFragmentX86 body = StringFragment {body :: body} deriving (Show)

data ProgramFragmentX86 body = Proc (ProcedureX86 body) | String (StringFragmentX86 body) deriving (Show)

instance HasField "procedure" (ProgramFragmentX86 body) (Maybe (ProcedureX86 body)) where
  getField (Proc procedure) = Just procedure
  getField _ = Nothing

instance HasField "string" (ProgramFragmentX86 body) (Maybe (StringFragmentX86 body)) where
  getField (Compiler.Backend.X86.Frame.String string) = Just string
  getField _ = Nothing

instance HasField "body" (ProgramFragmentX86 body) body where
  getField (Proc procedure) = procedure.body
  getField (Compiler.Backend.X86.Frame.String string) = string.body

instance Frame.Frame Frame where
  type Access Frame = Access
  newFrame = newFrame
  name frame = frame.name
  formals frame = frame.parameters
  allocLocal = allocateLocal
  fp = rbp
  rv = rax
  exp = exp
  wordSize = wordSize
  externalCall = externalCall
  procEntryExit1 frame stm = parameterReceiving frame `IR.Seq` stm -- TODO: callee save statement

type FrameEff = State Frame

runFrameEff :: Eff (("frame" >: FrameEff) ': xs) a -> Frame -> Eff xs (a, Frame)
runFrameEff = runStateEff @"frame"

getFrameEff :: (Lookup xs "frame" FrameEff) => Eff xs Frame
getFrameEff = getEff #frame

modifyFrameEff :: (Lookup xs "frame" FrameEff) => (Frame -> Frame) -> Eff xs ()
modifyFrameEff = modifyEff #frame

allocateParameterEff :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => Bool -> Eff xs ()
allocateParameterEff escape = do
  frame <- getEff #frame
  frame' <- allocateParameter frame escape
  putEff #frame frame'

allocateLocalEff :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => Bool -> Eff xs Access
allocateLocalEff escape = do
  frame <- getEff #frame
  (frame', access) <- allocateLocal frame escape
  putEff #frame frame'
  pure access

allocateNonEscapedLocalEff :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => Eff xs U.Temp
allocateNonEscapedLocalEff = allocateLocalEff False >>= \case InRegister t -> pure t; _ -> undefined

allocateEscapedLocalEff :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => Eff xs Int
allocateEscapedLocalEff = allocateLocalEff True >>= \case InFrame i -> pure i; _ -> undefined
