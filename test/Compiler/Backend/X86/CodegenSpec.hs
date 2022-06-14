module Compiler.Backend.X86.CodegenSpec (spec) where

import Compiler.Backend.X86.Arch
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.Frame (emptyFrame, r8, r9, rax, rbp, rcx, rdi, rdx, rip, rsi, rsp)
import Compiler.Backend.X86.IntermediateMock (IntermediateMock (IntermediateMock))
import Compiler.Backend.X86.Liveness qualified as L
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible.Effect (leaveEff)
import RIO
import RIO.List.Partial
import Test.Hspec

spec :: Spec
spec = codegenSpec

codegenSpec :: Spec
codegenSpec = describe "codegen spec" $ do
  it "Main prologue" $ do
    let blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Const 0)) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    take 3 result
      `shouldBe` [ L.Label {label = fromUniqueLabel blockLabel, val = Label (fromUniqueLabel blockLabel)},
                   L.Instruction {src = [], dst = [], val = PushRegister rbp},
                   L.Instruction {src = [], dst = [], val = MovRegister rsp rbp}
                 ]

  it "Main epilogue" $ do
    let blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Const 0)) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    drop (length result - 2) result
      `shouldBe` [ L.Instruction {src = [], dst = [], val = Leave},
                   L.Instruction {src = [], dst = [], val = Ret}
                 ]

  it "Move Temp Const -> mov $0x0 %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.Const 0)) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [t], val = MovImmediate 0 t}
                 ]

  it "Move Temp Temp -> mov %rbx %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.Temp t')) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t'], dst = [t], val = MovRegister t' t}
                 ]

  it "Move Temp Name -> lea label %rip %rax" $ do
    let t = U.Temp "t" (U.Unique 0)
        t' = U.Temp "t" (U.Unique 10)
        label = U.Label "label" (U.Unique 1)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t') (IR.Name label)) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [t], val = Lea (fromUniqueLabel label) rip t},
                   L.Instruction {src = [t], dst = [t'], val = MovRegister t t'}
                 ]

  it "Move Temp (Temp + Temp) -> mov %rbx %rax; add %rcx %rax; mov %rax %rdx" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        t'' = U.Temp "t" (U.Unique 12)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t') (IR.Temp t''))) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t'], dst = [dst], val = MovRegister t' dst},
                   L.Instruction {src = [dst, t''], dst = [dst], val = AddRegister t'' dst},
                   L.Instruction {src = [dst], dst = [t], val = MovRegister dst t}
                 ]

  it "Mem (Const 0) -> mov $0x0 %rax" $ do
    let dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.Const 0))) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [dst], val = MovLoad (Memory 0) dst}
                 ]

  it "Mem (Const 4 + Temp) -> mov $0x0 %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp t)))) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovLoadIndirect 4 t dst}
                 ]

  it "Mem (Temp + Temp) -> mov (%rbx, %rcx, 1) %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t')))) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [dst], val = MovLoadDisplacement 0 t t' 1 dst}
                 ]

  it "Mem (4 + Temp + Temp) -> mov $0x4(%rbx, %rcx, 1) %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))))) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [dst], val = MovLoadDisplacement 4 t t' 1 dst}
                 ]

  it "Move (Mem (Const 0)) Temp -> mov %rax $0x0" $ do
    let t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Mem (IR.Const 0)) (IR.Temp t)) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = MovStore t (Memory 0)}
                 ]

  it "Move (Mem (4 + Temp)) Temp -> mov %rax $0x4(%rbx)" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp t))) (IR.Temp t')) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = MovStoreIndirect t' 4 t}
                 ]

  it "Move (Mem (Temp + Temp)) Temp -> mov %rax %r1; add %rbx %r1; mov %rcx $0x0(%r1)" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        t'' = U.Temp "t" (U.Unique 12)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))) (IR.Temp t'')) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovRegister t dst},
                   L.Instruction {src = [dst, t'], dst = [dst], val = AddRegister t' dst},
                   L.Instruction {src = [dst, t''], dst = [], val = MovStoreIndirect t'' 0 dst}
                 ]

  it "Temp + 1 -> add %rbx %r1" $ do
    let t = U.Temp "t" (U.Unique 10)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovRegister t dst},
                   L.Instruction {src = [dst], dst = [dst], val = AddImmediate 1 dst}
                 ]

  it "Temp + Temp -> mov %rax %r1; add %rbx %r1" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovRegister t dst},
                   L.Instruction {src = [dst, t'], dst = [dst], val = AddRegister t' dst}
                 ]

  it "Jump Name -> jump label" $ do
    let label = U.Label "label" (U.Unique 0)
        labels = [label]
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Jump (IR.Name label) labels) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Jump {jumps = fromUniqueLabel <$> labels, val = Jump (fromUniqueLabel label)}
                 ]

  it "CJump (Temp == 1) label1 label2 -> cmp %rax $0x1; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Const 1) true false) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = CmpImmediate t 1},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]

  it "CJump (Temp == Temp) label1 label2 -> cmp %rax %rbx; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Temp t') true false) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = CmpRegister t t'},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]
  it "CJump (Temp == 1) label1 label2 -> cmp %rax $0x1; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Const 1) true false) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = CmpImmediate t 1},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]

  it "CJump (Temp == Temp) label1 label2 -> cmp %rax %rbx; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Temp t') true false) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = CmpRegister t t'},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]

  it "CJump (Temp < 1) label1 label2 -> cmp %rax $0x1; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Lt (IR.Temp t) (IR.Const 1) true false) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = CmpImmediate t 1},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfLessThan (fromUniqueLabel true)}
                 ]

  it "CJump (Temp < Temp) label1 label2 -> cmp %rax %rbx; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Lt (IR.Temp t) (IR.Temp t') true false) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = CmpRegister t t'},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfLessThan (fromUniqueLabel true)}
                 ]

  it "Label -> label: " $ do
    let label = U.Label "label" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Label label) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Label {label = fromUniqueLabel label, val = Label (fromUniqueLabel label)}
                 ]

  it "f(1,2,3,4,5,6,7,8,9,10) -> mov $0x1 %rax; ...; mov %rax %rdi; ...; mov %rcx 8(%rbp); ...; call f" $ do
    let f = U.Label "f" (U.Unique 0)
        temps = U.Temp "t" . U.Unique <$> [0 .. 20]
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Call (IR.Name f) [IR.Const 1, IR.Const 2, IR.Const 3, IR.Const 4, IR.Const 5, IR.Const 6, IR.Const 7, IR.Const 8, IR.Const 9, IR.Const 10])) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [temps !! 0], val = MovImmediate 1 (temps !! 0)},
                   L.Instruction {src = [], dst = [temps !! 1], val = MovImmediate 2 (temps !! 1)},
                   L.Instruction {src = [], dst = [temps !! 2], val = MovImmediate 3 (temps !! 2)},
                   L.Instruction {src = [], dst = [temps !! 3], val = MovImmediate 4 (temps !! 3)},
                   L.Instruction {src = [], dst = [temps !! 4], val = MovImmediate 5 (temps !! 4)},
                   L.Instruction {src = [], dst = [temps !! 5], val = MovImmediate 6 (temps !! 5)},
                   L.Instruction {src = [], dst = [temps !! 6], val = MovImmediate 7 (temps !! 6)},
                   L.Instruction {src = [], dst = [temps !! 7], val = MovImmediate 8 (temps !! 7)},
                   L.Instruction {src = [], dst = [temps !! 8], val = MovImmediate 9 (temps !! 8)},
                   L.Instruction {src = [], dst = [temps !! 9], val = MovImmediate 10 (temps !! 9)},
                   L.Instruction {src = [temps !! 0], dst = [rdi], val = MovRegister (temps !! 0) rdi},
                   L.Instruction {src = [temps !! 1], dst = [rsi], val = MovRegister (temps !! 1) rsi},
                   L.Instruction {src = [temps !! 2], dst = [rdx], val = MovRegister (temps !! 2) rdx},
                   L.Instruction {src = [temps !! 3], dst = [rcx], val = MovRegister (temps !! 3) rcx},
                   L.Instruction {src = [temps !! 4], dst = [r8], val = MovRegister (temps !! 4) r8},
                   L.Instruction {src = [temps !! 5], dst = [r9], val = MovRegister (temps !! 5) r9},
                   L.Instruction {src = [temps !! 6], dst = [], val = MovStoreIndirect (temps !! 6) 8 rbp},
                   L.Instruction {src = [temps !! 7], dst = [], val = MovStoreIndirect (temps !! 7) 16 rbp},
                   L.Instruction {src = [temps !! 8], dst = [], val = MovStoreIndirect (temps !! 8) 24 rbp},
                   L.Instruction {src = [temps !! 9], dst = [], val = MovStoreIndirect (temps !! 9) 32 rbp},
                   L.Instruction {src = take 10 temps, dst = [rax], val = Call (Label' "fu0")}
                 ]

  it "Move Temp (1+1); f(Temp) -> " $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 0)
        t'' = U.Temp "t" (U.Unique 1)
        f = U.Label "f" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Const 3) (IR.Const 2)) `IR.Seq` IR.Exp (IR.Call (IR.Name f) [IR.Temp t])) (emptyFrame blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [t'], val = MovImmediate 3 t'},
                   L.Instruction {src = [t'], dst = [t''], val = MovRegister t' t''},
                   L.Instruction {src = [t''], dst = [t''], val = AddImmediate 2 t''},
                   L.Instruction {src = [t''], dst = [t], val = MovRegister t'' t},
                   L.Instruction {src = [t], dst = [rdi], val = MovRegister t rdi},
                   L.Instruction {src = [t], dst = [rax], val = Call (Label' "fu0")}
                 ]

takeMainBlockBody :: [L.ControlFlow U.Temp (Assembly U.Temp)] -> [L.ControlFlow U.Temp (Assembly U.Temp)]
takeMainBlockBody flows = drop 3 (take (length flows - 2) flows)
