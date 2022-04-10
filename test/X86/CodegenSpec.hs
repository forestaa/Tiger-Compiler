module X86.CodegenSpec (spec) where

import Data.Extensible.Effect (leaveEff)
import IR qualified
import RIO
import RIO.List.Partial
import Test.Hspec
import Unique qualified as U
import X86.Arch
import X86.Codegen (codegen)
import X86.Liveness qualified as L

spec :: Spec
spec = codegenSpec

codegenSpec :: Spec
codegenSpec = describe "codegen spec" $ do
  it "Const -> mov $0x0 %rax" $ do
    let t = U.Temp "t" (U.Unique 0)
        stms = [IR.Exp (IR.Const 0)]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Instruction {src = [], dst = [t], val = MovImmediate 0 t}]

  it "Move Temp Const -> mov $0x0 %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        stms = [IR.Move (IR.Temp t) (IR.Const 0)]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Instruction {src = [], dst = [t], val = MovImmediate 0 t}]

  it "Move Temp Temp -> mov %rbx %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        stms = [IR.Move (IR.Temp t) (IR.Temp t')]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Instruction {src = [t'], dst = [t], val = MovRegister t' t}]

  it "Move Temp (Temp + Temp) -> mov %rbx %rax; add %rcx %rax; mov %rax %rdx" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        t'' = U.Temp "t" (U.Unique 12)
        dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t') (IR.Temp t''))]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t'], dst = [dst], val = MovRegister t' dst},
                   L.Instruction {src = [dst, t''], dst = [dst], val = AddRegister t'' dst},
                   L.Instruction {src = [dst], dst = [t], val = MovRegister dst t}
                 ]

  it "Mem (Const 0) -> mov $0x0 %rax" $ do
    let dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Exp (IR.Mem (IR.Const 0))]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Instruction {src = [], dst = [dst], val = MovLoad (Memory 0) dst}]

  it "Mem (Const 4 + Temp) -> mov $0x0 %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp t)))]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Instruction {src = [t], dst = [dst], val = MovLoadIndirect 4 t dst}]

  it "Mem (Temp + Temp) -> mov (%rbx, %rcx, 1) %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t')))]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [L.Instruction {src = [t, t'], dst = [dst], val = MovLoadDisplacement 0 t t' 1 dst}]

  it "Mem (4 + Temp + Temp) -> mov $0x4(%rbx, %rcx, 1) %rax" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Exp (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))))]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [L.Instruction {src = [t, t'], dst = [dst], val = MovLoadDisplacement 4 t t' 1 dst}]

  it "Move (Mem (Const 0)) Temp -> mov %rax $0x0" $ do
    let t = U.Temp "t" (U.Unique 10)
        stms = [IR.Move (IR.Mem (IR.Const 0)) (IR.Temp t)]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Instruction {src = [t], dst = [], val = MovStore t (Memory 0)}]

  it "Move (Mem (4 + Temp)) Temp -> mov %rax $0x4(%rbx)" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        stms = [IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp t))) (IR.Temp t')]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Instruction {src = [t, t'], dst = [], val = MovStoreIndirect t' 4 t}]

  it "Move (Mem (Temp + Temp)) Temp -> mov %rax %r1; add %rbx %r1; mov %rcx $0x0(%r1)" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        t'' = U.Temp "t" (U.Unique 12)
        dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))) (IR.Temp t'')]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovRegister t dst},
                   L.Instruction {src = [dst, t'], dst = [dst], val = AddRegister t' dst},
                   L.Instruction {src = [dst, t''], dst = [], val = MovStoreIndirect t'' 0 dst}
                 ]

  it "Temp + 1 -> add %rbx %r1" $ do
    let t = U.Temp "t" (U.Unique 10)
        dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Exp (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovRegister t dst},
                   L.Instruction {src = [dst], dst = [dst], val = AddImmediate 1 dst}
                 ]

  it "Temp + Temp -> mov %rax %r1; add %rbx %r1" $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        dst = U.Temp "t" (U.Unique 0)
        stms = [IR.Exp (IR.BinOp IR.Plus (IR.Temp t) (IR.Temp t'))]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t], dst = [dst], val = MovRegister t dst},
                   L.Instruction {src = [dst, t'], dst = [dst], val = AddRegister t' dst}
                 ]

  it "Jump Name -> jump label" $ do
    let label = U.Label "label" (U.Unique 0)
        labels = [label]
        stms = [IR.Jump (IR.Name label) labels]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [L.Jump {jumps = fromUniqueLabel <$> labels, val = Jump (fromUniqueLabel label)}]

  it "CJump (Temp == 1) label1 label2 -> cmp %rax $0x1; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        stms = [IR.CJump IR.Eq (IR.Temp t) (IR.Const 1) true false]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = CmpImmediate t 1},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]

  it "CJump (Temp == Temp) label1 label2 -> cmp %rax %rbx; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        stms = [IR.CJump IR.Eq (IR.Temp t) (IR.Temp t') true false]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = CmpRegister t t'},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]
  it "CJump (Temp == 1) label1 label2 -> cmp %rax $0x1; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        stms = [IR.CJump IR.Eq (IR.Temp t) (IR.Const 1) true false]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = CmpImmediate t 1},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]

  it "CJump (Temp == Temp) label1 label2 -> cmp %rax %rbx; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        stms = [IR.CJump IR.Eq (IR.Temp t) (IR.Temp t') true false]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = CmpRegister t t'},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfEqual (fromUniqueLabel true)}
                 ]

  it "CJump (Temp < 1) label1 label2 -> cmp %rax $0x1; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        stms = [IR.CJump IR.Lt (IR.Temp t) (IR.Const 1) true false]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t], dst = [], val = CmpImmediate t 1},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfLessThan (fromUniqueLabel true)}
                 ]

  it "CJump (Temp < Temp) label1 label2 -> cmp %rax %rbx; je label1" $ do
    let true = U.Label "true" (U.Unique 0)
        false = U.Label "false" (U.Unique 0)
        t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 11)
        stms = [IR.CJump IR.Lt (IR.Temp t) (IR.Temp t') true false]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [t, t'], dst = [], val = CmpRegister t t'},
                   L.CJump {jumps = [fromUniqueLabel true], val = JumpIfLessThan (fromUniqueLabel true)}
                 ]

  it "Label -> label: " $ do
    let label = U.Label "label" (U.Unique 0)
        stms = [IR.Label label]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result `shouldBe` [L.Label {label = fromUniqueLabel label, val = Label (fromUniqueLabel label)}]

  it "f(1,2,3,4,5,6,7,8,9,10) -> mov $0x1 %rax; ...; mov %rax %rdi; ...; mov %rcx 8(%rbp); ...; call f" $ do
    let f = U.Label "f" (U.Unique 0)
        stms = [IR.Exp (IR.Call (IR.Name f) [IR.Const 1, IR.Const 2, IR.Const 3, IR.Const 4, IR.Const 5, IR.Const 6, IR.Const 7, IR.Const 8, IR.Const 9, IR.Const 10])]
        temps = U.Temp "t" . U.Unique <$> [0 .. 20]
        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
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
                   L.Instruction {src = [temps !! 6], dst = [], val = MovStoreIndirect (temps !! 6) 8 (U.newStringTemp "fp")},
                   L.Instruction {src = [temps !! 7], dst = [], val = MovStoreIndirect (temps !! 7) 16 (U.newStringTemp "fp")},
                   L.Instruction {src = [temps !! 8], dst = [], val = MovStoreIndirect (temps !! 8) 24 (U.newStringTemp "fp")},
                   L.Instruction {src = [temps !! 9], dst = [], val = MovStoreIndirect (temps !! 9) 32 (U.newStringTemp "fp")},
                   L.Instruction {src = take 10 temps, dst = [U.newStringTemp "RAX"], val = Call (Label' "fu0")}
                 ]

  it "Move Temp (1+1); f(Temp) -> " $ do
    let t = U.Temp "t" (U.Unique 10)
        t' = U.Temp "t" (U.Unique 0)
        t'' = U.Temp "t" (U.Unique 1)
        f = U.Label "f" (U.Unique 0)
        stms = [IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Const 3) (IR.Const 2)), IR.Exp (IR.Call (IR.Name f) [IR.Temp t])]

        result = leaveEff . U.evalUniqueEff @"temp" $ codegen stms
    result
      `shouldBe` [ L.Instruction {src = [], dst = [t'], val = MovImmediate 2 t'},
                   L.Instruction {src = [t'], dst = [t''], val = MovRegister t' t''},
                   L.Instruction {src = [t''], dst = [t''], val = AddImmediate 3 t''},
                   L.Instruction {src = [t''], dst = [t], val = MovRegister t'' t},
                   L.Instruction {src = [t], dst = [U.newStringTemp "RDI"], val = MovRegister t (U.newStringTemp "RDI")},
                   L.Instruction {src = [t], dst = [U.newStringTemp "RAX"], val = Call (Label' "fu0")}
                 ]
