module Compiler.Backend.X86.Codegen (codegen) where

import Compiler.Backend.X86.Arch
import Compiler.Backend.X86.Frame
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..))
import Compiler.Intermediate (Intermediate, processIntermediate)
import Compiler.Intermediate.Frame qualified as F (Frame (..), Procedure (..), ProgramFragment (..), ProgramFragments (..), StringFragment (..))
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U (Label, Temp, UniqueEff, externalLabel)
import Data.Extensible (Lookup, type (>:))
import Data.Extensible.Effect (Eff, castEff)
import Data.List (singleton)
import RIO
import RIO.List.Partial qualified as List (head)
import RIO.Text qualified as T (length)
import RIO.Text.Partial qualified as T (replace)

codegen :: forall im xs. (Lookup xs "label" U.UniqueEff, Lookup xs "temp" U.UniqueEff, Intermediate im) => F.ProgramFragments Frame -> Eff xs [ProgramFragmentX86 [L.ControlFlow U.Temp (Assembly U.Temp)]]
codegen fragments = do
  flows1 <- mapM (codegenFragment @im) fragments.fragments
  flows2 <- codegenMain @im fragments.main
  pure $ flows1 ++ [flows2]

codegenMain :: forall im xs. (Lookup xs "label" U.UniqueEff, Lookup xs "temp" U.UniqueEff, Intermediate im) => F.ProgramFragment Frame -> Eff xs (ProgramFragmentX86 [L.ControlFlow U.Temp (Assembly U.Temp)])
codegenMain (F.Proc procedure) = do
  let externalLabel = U.externalLabel "tigermain"
  codegenFragment @im (F.Proc (procedure {F.frame = procedure.frame {name = externalLabel}}))
codegenMain _ = undefined

codegenFragment :: forall im xs. (Lookup xs "label" U.UniqueEff, Lookup xs "temp" U.UniqueEff, Intermediate im) => F.ProgramFragment Frame -> Eff xs (ProgramFragmentX86 [L.ControlFlow U.Temp (Assembly U.Temp)])
codegenFragment (F.Proc procedure) = do
  procedure <- processIntermediate @im $ procedure {F.body = IR.Label (F.name procedure.frame) `IR.Seq` procedure.body}
  (body, frame) <- castEff . flip runFrameEff procedure.frame $ do
    body <- concat <$> mapM codegenStm procedure.body :: Eff '["frame" >: FrameEff, "label" >: U.UniqueEff, "temp" >: U.UniqueEff] [L.ControlFlow U.Temp (Assembly U.Temp)]
    frame <- getFrameEff
    pure $ [L.Meta {val = Text}, L.Meta {val = Global (fromUniqueLabel frame.name)}, L.Meta {val = Type (fromUniqueLabel frame.name) Function}] ++ body
  pure . Proc $ Procedure {body = body, frame = frame}
codegenFragment (F.String string) = Compiler.Backend.X86.Frame.String . StringFragment <$> codegenString string.name string.text

codegenStm :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => IR.Stm -> Eff xs [L.ControlFlow U.Temp (Assembly U.Temp)]
codegenStm (IR.Move (IR.Mem (IR.BinOp IR.Plus e1 (IR.Const i))) e2) = codegenStm (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const i) e1)) e2)
codegenStm (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const i) e1)) e2) = do
  (flows1, t1) <- codegenExp e1
  (flows2, t2) <- codegenExp e2
  pure $ flows1 ++ flows2 ++ [L.Instruction {src = [t1, t2], dst = [], val = MovStoreIndirect t2 i t1}]
codegenStm (IR.Move (IR.Mem (IR.Const i)) e) = do
  (flows, t) <- codegenExp e
  pure $ flows ++ [L.Instruction {src = [t], dst = [], val = MovStore t (Memory i)}]
codegenStm (IR.Move (IR.Mem e1) e2) = codegenStm (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) e1)) e2)
codegenStm (IR.Move (IR.Temp t) (IR.Const i)) = pure [L.Instruction {src = [], dst = [t], val = MovImmediate i t}]
codegenStm (IR.Move (IR.Temp t1) (IR.Temp t2)) = pure [L.Move {src = [t2], dst = [t1], val = MovRegister t2 t1}]
codegenStm (IR.Move (IR.Temp t) e) = do
  (flows', t') <- codegenExp e
  pure $ flows' ++ [L.Move {src = [t'], dst = [t], val = MovRegister t' t}]
codegenStm (IR.Move _ _) = undefined
codegenStm (IR.Exp e) = fst <$> codegenExp e
codegenStm (IR.Jump (IR.Name label) labels) = pure [L.Jump {jumps = fromUniqueLabel <$> labels, val = Jump (fromUniqueLabel label)}]
codegenStm (IR.Jump _ _) = undefined
codegenStm (IR.CJump op e (IR.Const i) true _) = do
  (flows, t) <- codegenExp e
  pure $
    flows
      ++ [ L.Instruction {src = [t], dst = [], val = CmpImmediate t i},
           L.CJump {jumps = [fromUniqueLabel true], val = (jumpInstr op) (fromUniqueLabel true)}
         ]
codegenStm (IR.CJump op e1 e2 true _) = do
  (flows1, t1) <- codegenExp e1
  (flows2, t2) <- codegenExp e2
  pure $
    flows1
      ++ flows2
      ++ [ L.Instruction {src = [t1, t2], dst = [], val = CmpRegister t2 t1},
           L.CJump {jumps = [fromUniqueLabel true], val = (jumpInstr op) (fromUniqueLabel true)}
         ]
-- codegenStm (IR.Seq s1 s2) = (++) <$> codegenStm s1 <*> codegenStm s2
codegenStm (IR.Seq _ _) = undefined
codegenStm (IR.Label label) = pure [L.Label {label' = label', val = Label label'}]
  where
    label' = fromUniqueLabel label

codegenExp :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => IR.Exp -> Eff xs ([L.ControlFlow U.Temp (Assembly U.Temp)], U.Temp)
codegenExp (IR.Const i) = do
  t <- allocateNonEscapedLocalEff
  pure ([L.Instruction {src = [], dst = [t], val = MovImmediate i t}], t)
codegenExp (IR.Name label) = do
  t <- allocateNonEscapedLocalEff
  pure ([L.Instruction {src = [rip], dst = [t], val = Lea (fromUniqueLabel label) rip t}], t)
codegenExp (IR.Temp t) = pure ([], t)
codegenExp (IR.BinOp op e (IR.Const i)) = do
  (flows, t) <- codegenExp e
  t' <- allocateNonEscapedLocalEff
  (flows'', t'') <- codegenBinOpImmediate op t' i
  pure (flows ++ [L.Move {src = [t], dst = [t'], val = MovRegister t t'}] ++ flows'', t'')
-- codegenExp (IR.BinOp op (IR.Const i) e) = codegenExp (IR.BinOp op e (IR.Const i)) -- TODO: not for uncommutative binary operator
codegenExp (IR.BinOp op e1 e2) = do
  (flows1, t1) <- codegenExp e1
  (flows2, t2) <- codegenExp e2
  t1' <- allocateNonEscapedLocalEff
  (flows3, t3) <- codegenBinOp op t1' t2
  pure (flows1 ++ flows2 ++ [L.Move {src = [t1], dst = [t1'], val = MovRegister t1 t1'}] ++ flows3, t3)
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2))) = do
  (flows1, t1) <- codegenExp e1
  (flows2, t2) <- codegenExp e2
  t <- allocateNonEscapedLocalEff
  pure (flows1 ++ flows2 ++ [L.Instruction {src = [t1, t2], dst = [t], val = MovLoadDisplacement i t1 t2 1 t}], t)
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.BinOp IR.Plus e1 (IR.Const i)) e2)) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus e1 (IR.BinOp IR.Plus (IR.Const i) e2))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.BinOp IR.Plus e1 e2) (IR.Const i))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus e1 (IR.BinOp IR.Plus e2 (IR.Const i)))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.BinOp IR.Plus (IR.Const i) e1) e2)) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) e)) = do
  (flows, t) <- codegenExp e
  t' <- allocateNonEscapedLocalEff
  pure (flows ++ [L.Instruction {src = [t], dst = [t'], val = MovLoadIndirect i t t'}], t')
codegenExp (IR.Mem (IR.BinOp IR.Plus e (IR.Const i))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) e))
codegenExp (IR.Mem (IR.BinOp IR.Plus e1 e2)) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.Const i)) = do
  t <- allocateNonEscapedLocalEff
  pure ([L.Instruction {src = [], dst = [t], val = MovLoad (Memory i) t}], t)
codegenExp (IR.Mem e) = do
  (flow, t) <- codegenExp e
  t' <- allocateNonEscapedLocalEff
  pure (flow ++ [L.Instruction {src = [t], dst = [t'], val = MovLoadIndirect 0 t t'}], t')
codegenExp (IR.Call (IR.Name f) es) = do
  (flows, dsts) <- codegenParameters es
  pure (flows ++ [L.Instruction {src = dsts, dst = callerSaveTempRegisters, val = Call (fromUniqueLabel f)}], rax)
codegenExp (IR.Call _ _) = undefined
codegenExp (IR.ESeq _ _) = undefined

codegenParameters :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => [IR.Exp] -> Eff xs ([L.ControlFlow U.Temp (Assembly U.Temp)], [U.Temp])
codegenParameters es = do
  (flows, dsts) <- foldM (\(flows, dsts) -> fmap (((++) flows) *** ((++) dsts . singleton)) . codegenExp) ([], []) es
  let parameterPassingInstrs = zipWith ($) (parameterPassingByRegisters ++ parameterPassingByMemory) dsts
  pure (flows ++ parameterPassingInstrs, dsts)
  where
    parameterPassingByRegisters = (\register dst -> L.Move {src = [dst], dst = [register], val = MovRegister dst register}) <$> parameterTempRegisters
    parameterPassingByMemory = (\i dst -> L.Instruction {src = [dst, rbp], dst = [], val = MovStoreIndirect dst ((i - 6) * wordSize) rbp}) <$> [7 ..] -- TODO: use pushq

jumpInstr :: forall register. IR.RelOp -> Label -> Assembly register
jumpInstr IR.Eq = JumpIfEqual
jumpInstr IR.Ne = JumpIfNotEqual
jumpInstr IR.Lt = JumpIfLessThan
jumpInstr IR.Gt = JumpIfGreaterThan
jumpInstr IR.Le = JumpIfEqualOrLessThan
jumpInstr IR.Ge = JumpIfEqualOrGreaterThan

codegenBinOpImmediate :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => IR.BinOp -> U.Temp -> Int -> Eff xs ([L.ControlFlow U.Temp (Assembly U.Temp)], U.Temp)
codegenBinOpImmediate IR.Plus t i = pure ([L.Instruction {src = [t], dst = [t], val = AddImmediate i t}], t)
codegenBinOpImmediate IR.Minus t i = pure ([L.Instruction {src = [t], dst = [t], val = SubImmediate i t}], t)
codegenBinOpImmediate IR.Mul t i = pure ([L.Instruction {src = [t], dst = [t], val = MulImmediate i t t}], t)
codegenBinOpImmediate IR.Div t i = do
  t' <- allocateNonEscapedLocalEff
  pure
    ( [ L.Instruction {src = [], dst = [t'], val = MovImmediate i t'},
        L.Move {src = [t], dst = [rax], val = MovRegister t rax},
        L.Instruction {src = [rax], dst = [rax, rdx], val = Cqo},
        L.Instruction {src = [t', rax, rdx], dst = [rax, rdx], val = DivRegister t'}
      ],
      rax
    )

codegenBinOp :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => IR.BinOp -> U.Temp -> U.Temp -> Eff xs ([L.ControlFlow U.Temp (Assembly U.Temp)], U.Temp)
codegenBinOp IR.Plus t1 t2 = pure ([L.Instruction {src = [t1, t2], dst = [t1], val = AddRegister t2 t1}], t1)
codegenBinOp IR.Minus t1 t2 = pure ([L.Instruction {src = [t1, t2], dst = [t1], val = SubRegister t2 t1}], t1)
codegenBinOp IR.Mul t1 t2 = pure ([L.Instruction {src = [t1, t2], dst = [t1], val = MulRegister t2 t1}], t1)
codegenBinOp IR.Div t1 t2 = do
  pure
    ( [ L.Move {src = [t1], dst = [rax], val = MovRegister t1 rax},
        L.Instruction {src = [rax], dst = [rax, rdx], val = Cqo},
        L.Instruction {src = [rax, rdx, t2], dst = [rax, rdx], val = DivRegister t2}
      ],
      rax
    )

codegenString :: U.Label -> Text -> Eff xs [L.ControlFlow U.Temp (Assembly U.Temp)]
codegenString label string =
  pure $
    [ L.Meta {val = Text},
      L.Meta {val = Global (fromUniqueLabel label)},
      L.Meta {val = Data},
      L.Meta {val = Align 16}, -- TODO: should it be calculated? how?
      L.Meta {val = Type (fromUniqueLabel label) Object},
      L.Meta {val = Size (fromUniqueLabel label) totalSize},
      L.Label {label' = fromUniqueLabel label, val = Label (fromUniqueLabel label)},
      L.Meta {val = Quad stringSize},
      L.Meta {val = Compiler.Backend.X86.Arch.String string}
    ]
      ++ [L.Meta {val = Zero padding} | padding > 0]
  where
    stringLengthSize = wordSize
    stringSize = T.length (T.replace "\\n" "\n" string)
    totalSize = List.head [wordSize * i | i <- [1 ..], stringLengthSize + stringSize <= wordSize * i]
    padding = totalSize - stringLengthSize - stringSize - 1 -- Memo: why minus 1?
