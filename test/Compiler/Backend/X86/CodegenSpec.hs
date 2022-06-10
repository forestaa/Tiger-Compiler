module Compiler.Backend.X86.CodegenSpec (spec) where

import Compiler.Backend.X86.Arch
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.IntermediateMock (IntermediateMock (IntermediateMock))
import Compiler.Backend.X86.Liveness qualified as L
import Compiler.Frontend.FrameMock (FrameMock (..))
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
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Const 0)) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
        tempRbp = U.Temp "RBP" (U.Unique 0)
        tempRsp = U.Temp "RSP" (U.Unique 0)
    take 3 result
      `shouldBe` [ L.Label {label = fromUniqueLabel blockLabel, val = Label (fromUniqueLabel blockLabel)},
                   L.Instruction {src = [], dst = [], val = PushRegister tempRbp},
                   L.Instruction {src = [], dst = [], val = MovRegister tempRsp tempRbp}
                 ]

  it "Main epilogue" $ do
    let blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Const 0)) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    drop (length result - 2) result
      `shouldBe` [ L.Instruction {src = [], dst = [], val = Leave},
                   L.Instruction {src = [], dst = [], val = Ret}
                 ]

  it "Move Temp Const -> mov $0x0 %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.Const 0)) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [t], val = MovImmediate 0 t}
                 ]

  it "Move Temp Temp -> mov %rbx %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.Temp t')) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t'], dst = [t], val = MovRegister t' t}
                 ]

  it "Move Temp Name -> lea label %rip %rax" $ do
    let t = U.Temp "t" (U.Unique 0)
        t' = U.Temp "t" (U.Unique 10)
        rip = U.Temp "RIP" (U.Unique 0)
        label = U.Label "label" (U.Unique 1)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t') (IR.Name label)) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t') (IR.Temp t''))) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t'], dst = [dst], val = MovRegister t' dst},
                   L.Instruction {src = [dst, t''], dst = [dst], val = AddRegister t'' dst},
                   L.Instruction {src = [dst], dst = [t], val = MovRegister dst t}
                 ]

  it "Mem (Const 0) -> mov $0x0 %rax" $ do
    let dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.Const 0))) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [dst], val = MovLoad (Memory 0) dst}
                 ]

  it "Mem (Const 4 + Temp) -> mov $0x0 %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp t)))) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovLoadIndirect 4 t dst}
                 ]

  it "Mem (Temp + Temp) -> mov (%rbx, %rcx, 1) %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t')))) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [dst], val = MovLoadDisplacement 0 t t' 1 dst}
                 ]

  it "Mem (4 + Temp + Temp) -> mov $0x4(%rbx, %rcx, 1) %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))))) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [dst], val = MovLoadDisplacement 4 t t' 1 dst}
                 ]

  it "Move (Mem (Const 0)) Temp -> mov %rax $0x0" $ do
    let t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Mem (IR.Const 0)) (IR.Temp t)) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = MovStore t (Memory 0)}
                 ]

  it "Move (Mem (4 + Temp)) Temp -> mov %rax $0x4(%rbx)" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp t))) (IR.Temp t')) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))) (IR.Temp t'')) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovRegister t dst},
                   L.Instruction {src = [dst, t'], dst = [dst], val = AddRegister t' dst}
                 ]

  it "Jump Name -> jump label" $ do
    let label = U.Label "label" (U.Unique 0)
        labels = [label]
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Jump (IR.Name label) labels) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Jump {jumps = fromUniqueLabel <$> labels, val = Jump (fromUniqueLabel label)}
                 ]

  it "CJump (Temp == 1) label1 label2 -> cmp %rax $0x1; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Const 1) true false) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Temp t') true false) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Const 1) true false) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Eq (IR.Temp t) (IR.Temp t') true false) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Lt (IR.Temp t) (IR.Const 1) true false) (frameMock blockLabel), fragments = []}
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
        fragment = F.ProgramFragments {main = F.Proc (IR.CJump IR.Lt (IR.Temp t) (IR.Temp t') true false) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = CmpRegister t t'},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfLessThan (fromUniqueLabel true)}
                 ]

  it "Label -> label: " $ do
    let label = U.Label "label" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Label label) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Label {label = fromUniqueLabel label, val = Label (fromUniqueLabel label)}
                 ]

  it "f(1,2,3,4,5,6,7,8,9,10) -> mov $0x1 %rax; ...; mov %rax %rdi; ...; mov %rcx 8(%rbp); ...; call f" $ do
    let f = U.Label "f" (U.Unique 0)
        temps = U.Temp "t" . U.Unique <$> [0 .. 20]
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Exp (IR.Call (IR.Name f) [IR.Const 1, IR.Const 2, IR.Const 3, IR.Const 4, IR.Const 5, IR.Const 6, IR.Const 7, IR.Const 8, IR.Const 9, IR.Const 10])) (frameMock blockLabel), fragments = []}
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
                   L.Instruction {src = [temps !! 0], dst = [U.newStringTemp "RDI"], val = MovRegister (temps !! 0) (U.newStringTemp "RDI")},
                   L.Instruction {src = [temps !! 1], dst = [U.newStringTemp "RSI"], val = MovRegister (temps !! 1) (U.newStringTemp "RSI")},
                   L.Instruction {src = [temps !! 2], dst = [U.newStringTemp "RDX"], val = MovRegister (temps !! 2) (U.newStringTemp "RDX")},
                   L.Instruction {src = [temps !! 3], dst = [U.newStringTemp "RCX"], val = MovRegister (temps !! 3) (U.newStringTemp "RCX")},
                   L.Instruction {src = [temps !! 4], dst = [U.newStringTemp "R8"], val = MovRegister (temps !! 4) (U.newStringTemp "R8")},
                   L.Instruction {src = [temps !! 5], dst = [U.newStringTemp "R9"], val = MovRegister (temps !! 5) (U.newStringTemp "R9")},
                   L.Instruction {src = [temps !! 6], dst = [], val = MovStoreIndirect (temps !! 6) 8 (U.newStringTemp "RBP")},
                   L.Instruction {src = [temps !! 7], dst = [], val = MovStoreIndirect (temps !! 7) 16 (U.newStringTemp "RBP")},
                   L.Instruction {src = [temps !! 8], dst = [], val = MovStoreIndirect (temps !! 8) 24 (U.newStringTemp "RBP")},
                   L.Instruction {src = [temps !! 9], dst = [], val = MovStoreIndirect (temps !! 9) 32 (U.newStringTemp "RBP")},
                   L.Instruction {src = take 10 temps, dst = [U.newStringTemp "RAX"], val = Call (Label' "fu0")}
                 ]

  it "Move Temp (1+1); f(Temp) -> " $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 0)
        t'' = U.Temp "t" (U.Unique 1)
        f = U.Label "f" (U.Unique 0)
        blockLabel = U.Label "tigerMain" (U.Unique 0)
        fragment = F.ProgramFragments {main = F.Proc (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Const 3) (IR.Const 2)) `IR.Seq` IR.Exp (IR.Call (IR.Name f) [IR.Temp t])) (frameMock blockLabel), fragments = []}
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ codegen @IntermediateMock fragment
    takeMainBlockBody result
      `shouldBe` [ L.Instruction {src = [], dst = [t'], val = MovImmediate 3 t'},
                   L.Instruction {src = [t'], dst = [t''], val = MovRegister t' t''},
                   L.Instruction {src = [t''], dst = [t''], val = AddImmediate 2 t''},
                   L.Instruction {src = [t''], dst = [t], val = MovRegister t'' t},
                   L.Instruction {src = [t], dst = [U.newStringTemp "RDI"], val = MovRegister t (U.newStringTemp "RDI")},
                   L.Instruction {src = [t], dst = [U.newStringTemp "RAX"], val = Call (Label' "fu0")}
                 ]

frameMock :: U.Label -> FrameMock
frameMock label = FrameMock {name = label}

takeMainBlockBody :: [L.ControlFlow U.Temp (Assembly U.Temp)] -> [L.ControlFlow U.Temp (Assembly U.Temp)]
takeMainBlockBody flows = drop 3 (take (length flows - 2) flows)
