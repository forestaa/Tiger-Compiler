module Backend.X86.Codegen (codegen) where

import Backend.X86.Arch
import Backend.X86.Frame
import Backend.X86.Liveness qualified as L (ControlFlow (..))
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)
import Data.List (singleton)
import Intermediate.IR qualified as IR
import Intermediate.Unique (Temp, UniqueEff, newStringTemp, newTemp)
import RIO

codegen :: Lookup xs "temp" UniqueEff => [IR.Stm] -> Eff xs [L.ControlFlow Temp (Assembly Temp)]
codegen = fmap concat . mapM codegenStm

codegenStm :: Lookup xs "temp" UniqueEff => IR.Stm -> Eff xs [L.ControlFlow Temp (Assembly Temp)]
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
codegenStm (IR.Move (IR.Temp t1) (IR.Temp t2)) = pure [L.Instruction {src = [t2], dst = [t1], val = MovRegister t2 t1}]
codegenStm (IR.Move (IR.Temp t) e) = do
  (flows', t') <- codegenExp e
  pure $ flows' ++ [L.Instruction {src = [t'], dst = [t], val = MovRegister t' t}]
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
    flows1 ++ flows2
      ++ [ L.Instruction {src = [t1, t2], dst = [], val = CmpRegister t1 t2},
           L.CJump {jumps = [fromUniqueLabel true], val = (jumpInstr op) (fromUniqueLabel true)}
         ]
-- codegenStm (IR.Seq s1 s2) = (++) <$> codegenStm s1 <*> codegenStm s2
codegenStm (IR.Seq _ _) = undefined
codegenStm (IR.Label label) = pure [L.Label {label = label', val = Label label'}]
  where
    label' = fromUniqueLabel label

codegenExp :: Lookup xs "temp" UniqueEff => IR.Exp -> Eff xs ([L.ControlFlow Temp (Assembly Temp)], Temp)
codegenExp (IR.Const i) = do
  t <- newTemp
  pure ([L.Instruction {src = [], dst = [t], val = MovImmediate i t}], t)
codegenExp (IR.Name label) = do
  t <- newTemp
  pure ([L.Instruction {src = [], dst = [t], val = MovImmediateLabel (fromUniqueLabel label) t}], t)
codegenExp (IR.Temp t) = pure ([], t)
codegenExp (IR.BinOp op e (IR.Const i)) = do
  (flows, t) <- codegenExp e
  t' <- newTemp
  let flows' =
        flows
          ++ [ L.Instruction {src = [t], dst = [t'], val = MovRegister t t'},
               L.Instruction {src = [t'], dst = [t'], val = (binOpImmediateInstr op) i t'}
             ]
  pure (flows', t')
-- codegenExp (IR.BinOp op (IR.Const i) e) = codegenExp (IR.BinOp op e (IR.Const i)) -- TODO: not for uncommutative binary operator
codegenExp (IR.BinOp op e1 e2) = do
  (flows1, t1) <- codegenExp e1
  (flows2, t2) <- codegenExp e2
  t <- newTemp
  let flows =
        flows1 ++ flows2
          ++ [ L.Instruction {src = [t1], dst = [t], val = MovRegister t1 t},
               L.Instruction {src = [t, t2], dst = [t], val = (binOpInstr op) t2 t}
             ]
  pure (flows, t)
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2))) = do
  (flows1, t1) <- codegenExp e1
  (flows2, t2) <- codegenExp e2
  t <- newTemp
  pure (flows1 ++ flows2 ++ [L.Instruction {src = [t1, t2], dst = [t], val = MovLoadDisplacement i t1 t2 1 t}], t2)
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.BinOp IR.Plus e1 (IR.Const i)) e2)) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus e1 (IR.BinOp IR.Plus (IR.Const i) e2))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.BinOp IR.Plus e1 e2) (IR.Const i))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus e1 (IR.BinOp IR.Plus e2 (IR.Const i)))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.BinOp IR.Plus (IR.Const i) e1) e2)) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) e)) = do
  (flows, t) <- codegenExp e
  t' <- newTemp
  pure (flows ++ [L.Instruction {src = [t], dst = [t'], val = MovLoadIndirect i t t'}], t')
codegenExp (IR.Mem (IR.BinOp IR.Plus e (IR.Const i))) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const i) e))
codegenExp (IR.Mem (IR.BinOp IR.Plus e1 e2)) = codegenExp (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.BinOp IR.Plus e1 e2)))
codegenExp (IR.Mem (IR.Const i)) = do
  t <- newTemp
  pure ([L.Instruction {src = [], dst = [t], val = MovLoad (Memory i) t}], t)
codegenExp (IR.Mem e) = do
  (flow, t) <- codegenExp e
  t' <- newTemp
  pure (flow ++ [L.Instruction {src = [t], dst = [t'], val = MovLoadIndirect 0 t t'}], t')
codegenExp (IR.Call (IR.Name f) es) = do
  (flows, dsts) <- codegenParameters es
  let t = newStringTemp "RAX"
  pure (flows ++ [L.Instruction {src = dsts, dst = [t], val = Call (fromUniqueLabel f)}], t)
codegenExp (IR.Call _ _) = undefined
codegenExp (IR.ESeq _ _) = undefined

codegenParameters :: Lookup xs "temp" UniqueEff => [IR.Exp] -> Eff xs ([L.ControlFlow Temp (Assembly Temp)], [Temp])
codegenParameters es = do
  (flows, dsts) <- foldM (\(flows, dsts) -> fmap (((++) flows) *** ((++) dsts . singleton)) . codegenExp) ([], []) es
  let parameterPassingInstrs = zipWith ($) (parameterPassingByRegisters ++ parameterPassingByMemory) dsts
  pure (flows ++ parameterPassingInstrs, dsts)
  where
    parameterPassingByRegisters = (\register dst -> L.Instruction {src = [dst], dst = [register], val = MovRegister dst register}) <$> parameterTempRegisters
    parameterPassingByMemory = (\i dst -> L.Instruction {src = [dst], dst = [], val = MovStoreIndirect dst ((i - 6) * wordSize) bp}) <$> [7 ..] -- TODO: use pushq

jumpInstr :: forall register. IR.RelOp -> Label -> Assembly register
jumpInstr IR.Eq = JumpIfEqual
jumpInstr IR.Ne = JumpIfNotEqual
jumpInstr IR.Lt = JumpIfLessThan
jumpInstr IR.Gt = JumpIfGreaterThan
jumpInstr IR.Le = JumpIfEqualOrLessThan
jumpInstr IR.Ge = JumpIfEqualOrGreaterThan

binOpImmediateInstr :: forall register. IR.BinOp -> Int -> register -> Assembly register
binOpImmediateInstr IR.Plus = AddImmediate
binOpImmediateInstr IR.Minus = SubImmediate
binOpImmediateInstr _ = undefined

binOpInstr :: forall register. IR.BinOp -> register -> register -> Assembly register
binOpInstr IR.Plus = AddRegister
binOpInstr IR.Minus = SubRegister
binOpInstr _ = undefined
