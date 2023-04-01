module Compiler.Backend.X86.RegisterAllocation.CoalesceAllocationSpec where

import Compiler.Backend.X86.Arch (Assembly (AddImmediate, AddRegister, MovImmediate, MovLoadIndirect, MovRegister, MovStoreIndirect), Register (..), callerSaveRegisters)
import Compiler.Backend.X86.Frame (Frame (..), ProcedureX86 (..), allocateLocal, getAllocatedRegisters, newFrame, r10, r11, r12, r13, r14, r15, r8, r9, rax, rbp, rbx, rcx, rdi, rdx, rip, rsi, rsp)
import Compiler.Backend.X86.Liveness qualified as L
import Compiler.Backend.X86.RegisterAllocation (RegisterAllocation (..))
import Compiler.Backend.X86.RegisterAllocation.CoalesceAllocation (CoalesceAllocation)
import Compiler.Intermediate.Unique qualified as U (evalUniqueEff, newLabel)
import Data.Extensible.Effect (leaveEff)
import RIO
import RIO.List.Partial ((!!))
import Test.Hspec (Spec, describe, it, pending, pendingWith, shouldBe)

spec :: Spec
spec = do
  allocateRegistersSpec

allocateRegistersSpec :: Spec
allocateRegistersSpec = describe "allocateRegisters Spec" $ do
  it "simplify" $ do
    let result = leaveEff . U.evalUniqueEff @"temp" . U.evalUniqueEff @"label" $ do
          label <- U.newLabel
          frame <- newFrame label []
          frame <- foldM (\frame _ -> fst <$> allocateLocal frame False) frame [0 .. 4]
          let t = getAllocatedRegisters frame
              t0 = t !! 0
              t1 = t !! 1
              t2 = t !! 2
              t3 = t !! 3
              t4 = t !! 4
              body =
                [ L.Instruction {src = [], dst = [t0], val = MovImmediate 0 t0},
                  L.Instruction {src = [], dst = [t1], val = MovImmediate 1 t1},
                  L.Instruction {src = [], dst = [t2], val = MovImmediate 2 t2},
                  L.Instruction {src = [], dst = [t3], val = MovImmediate 3 t3},
                  L.Instruction {src = [], dst = [t4], val = MovImmediate 4 t4},
                  L.Instruction {src = [], dst = [t0], val = AddImmediate 1 t0},
                  L.Instruction {src = [t0, t1], dst = [t1], val = AddRegister t0 t1},
                  L.Instruction {src = [t1, t2], dst = [t2], val = AddRegister t1 t2},
                  L.Instruction {src = [t2, t3], dst = [t3], val = AddRegister t2 t3},
                  L.Instruction {src = [t3, t4], dst = [t4], val = AddRegister t3 t4}
                ]
          allocateRegisters @CoalesceAllocation Procedure {body = body, frame = frame}
    result.body
      `shouldBe` [ MovImmediate 0 RAX,
                   MovImmediate 1 RCX,
                   MovImmediate 2 RDX,
                   MovImmediate 3 RSI,
                   MovImmediate 4 RDI,
                   AddImmediate 1 RAX,
                   AddRegister RAX RCX,
                   AddRegister RCX RDX,
                   AddRegister RDX RSI,
                   AddRegister RSI RDI
                 ]

  it "move not between interfered node can be coalesced" $ do
    let result = leaveEff . U.evalUniqueEff @"temp" . U.evalUniqueEff @"label" $ do
          label <- U.newLabel
          frame <- newFrame label []
          frame <- foldM (\frame _ -> fst <$> allocateLocal frame False) frame [0 .. 1]
          let t = getAllocatedRegisters frame
              t0 = t !! 0
              t1 = t !! 1
              body =
                [ L.Instruction {src = [], dst = [t0], val = MovImmediate 0 t0},
                  L.Move {src = [t0], dst = [t1], val = MovRegister t0 t1}
                ]
          allocateRegisters @CoalesceAllocation Procedure {body = body, frame = frame}
    result.body
      `shouldBe` [ MovImmediate 0 RAX
                 ]

  it "a node can be coalesced with precolored node" $ do
    let result = leaveEff . U.evalUniqueEff @"temp" . U.evalUniqueEff @"label" $ do
          label <- U.newLabel
          frame <- newFrame label []
          frame <- foldM (\frame _ -> fst <$> allocateLocal frame False) frame [0]
          let t = getAllocatedRegisters frame
              t0 = t !! 0
              body =
                [ L.Instruction {src = [], dst = [t0], val = MovImmediate 0 t0},
                  L.Move {src = [t0], dst = [rbx], val = MovRegister t0 rbx}
                ]
          allocateRegisters @CoalesceAllocation Procedure {body = body, frame = frame}
    result.body
      `shouldBe` [ MovImmediate 0 RBX
                 ]

  it "move between precolored cannot be coalesced" $ do
    let result = leaveEff . U.evalUniqueEff @"temp" . U.evalUniqueEff @"label" $ do
          label <- U.newLabel
          frame <- newFrame label []
          let body =
                [ L.Move {src = [rax], dst = [rbx], val = MovRegister rax rbx}
                ]
          allocateRegisters @CoalesceAllocation Procedure {body = body, frame = frame}
    result.body
      `shouldBe` [ MovRegister RAX RBX
                 ]

  it "not coalesced move can be freezed and simplify" $ do
    pendingWith "too difficult to write testcase"

  it "possibly spilled, but colored" $ do
    pendingWith "too difficult to write testcase"

  it "spilled and startover" $ do
    let result = leaveEff . U.evalUniqueEff @"temp" . U.evalUniqueEff @"label" $ do
          label <- U.newLabel
          frame <- newFrame label []
          frame <- foldM (\frame _ -> fst <$> allocateLocal frame False) frame [0 .. length callerSaveRegisters]
          let t = getAllocatedRegisters frame
              t0 = t !! 0
              t1 = t !! 1
              t2 = t !! 2
              t3 = t !! 3
              t4 = t !! 4
              t5 = t !! 5
              t6 = t !! 6
              t7 = t !! 7
              t8 = t !! 8
              t9 = t !! 9
              body =
                [ L.Instruction {src = [], dst = [t0], val = MovImmediate 0 t0},
                  L.Instruction {src = [], dst = [t1], val = MovImmediate 1 t1},
                  L.Instruction {src = [], dst = [t2], val = MovImmediate 2 t2},
                  L.Instruction {src = [], dst = [t3], val = MovImmediate 3 t3},
                  L.Instruction {src = [], dst = [t4], val = MovImmediate 4 t4},
                  L.Instruction {src = [], dst = [t5], val = MovImmediate 5 t5},
                  L.Instruction {src = [], dst = [t6], val = MovImmediate 6 t6},
                  L.Instruction {src = [], dst = [t7], val = MovImmediate 7 t7},
                  L.Instruction {src = [], dst = [t8], val = MovImmediate 8 t8},
                  L.Instruction {src = [], dst = [t9], val = MovImmediate 9 t9},
                  L.Instruction {src = [t0], dst = [t0], val = AddImmediate 1 t0},
                  L.Instruction {src = [t0, t1], dst = [t1], val = AddRegister t0 t1},
                  L.Instruction {src = [t1, t2], dst = [t2], val = AddRegister t1 t2},
                  L.Instruction {src = [t2, t3], dst = [t3], val = AddRegister t2 t3},
                  L.Instruction {src = [t3, t4], dst = [t4], val = AddRegister t3 t4},
                  L.Instruction {src = [t4, t5], dst = [t5], val = AddRegister t4 t5},
                  L.Instruction {src = [t5, t6], dst = [t6], val = AddRegister t5 t6},
                  L.Instruction {src = [t6, t7], dst = [t7], val = AddRegister t6 t7},
                  L.Instruction {src = [t7, t8], dst = [t8], val = AddRegister t7 t8},
                  L.Instruction {src = [t8, t9], dst = [t9], val = AddRegister t8 t9}
                ]
          allocateRegisters @CoalesceAllocation Procedure {body = body, frame = frame}
    result.body
      `shouldBe` [ MovImmediate 0 RAX,
                   MovStoreIndirect RAX (-8) RBP,
                   MovImmediate 1 RAX,
                   MovStoreIndirect RAX (-16) RBP,
                   MovImmediate 2 RAX,
                   MovStoreIndirect RAX (-24) RBP,
                   MovImmediate 3 RCX,
                   MovImmediate 4 RDX,
                   MovImmediate 5 RSI,
                   MovImmediate 6 RDI,
                   MovImmediate 7 R8,
                   MovImmediate 8 R9,
                   MovImmediate 9 R10,
                   MovLoadIndirect (-8) RBP RAX,
                   AddImmediate 1 RAX,
                   MovStoreIndirect RAX (-8) RBP,
                   MovLoadIndirect (-8) RBP RAX,
                   MovLoadIndirect (-16) RBP R11,
                   AddRegister RAX R11,
                   MovStoreIndirect R11 (-16) RBP,
                   MovLoadIndirect (-16) RBP RAX,
                   MovLoadIndirect (-24) RBP R11,
                   AddRegister RAX R11,
                   MovStoreIndirect R11 (-24) RBP,
                   MovLoadIndirect (-24) RBP RAX,
                   AddRegister RAX RCX,
                   AddRegister RCX RDX,
                   AddRegister RDX RSI,
                   AddRegister RSI RDI,
                   AddRegister RDI R8,
                   AddRegister R8 R9,
                   AddRegister R9 R10
                 ]
    result.frame.head `shouldBe` -32

  it "precolored" $ do
    let result = leaveEff . U.evalUniqueEff @"temp" . U.evalUniqueEff @"label" $ do
          label <- U.newLabel
          frame <- newFrame label []
          let body =
                [ L.Instruction {src = [], dst = [r8], val = MovImmediate 1 r8},
                  L.Instruction {src = [], dst = [r9], val = MovImmediate 1 r9},
                  L.Instruction {src = [], dst = [r10], val = MovImmediate 1 r10},
                  L.Instruction {src = [], dst = [r11], val = MovImmediate 1 r11},
                  L.Instruction {src = [], dst = [r12], val = MovImmediate 1 r12},
                  L.Instruction {src = [], dst = [r13], val = MovImmediate 1 r13},
                  L.Instruction {src = [], dst = [r14], val = MovImmediate 1 r14},
                  L.Instruction {src = [], dst = [r15], val = MovImmediate 1 r15},
                  L.Instruction {src = [], dst = [rip], val = MovImmediate 1 rip},
                  L.Instruction {src = [], dst = [rax], val = MovImmediate 1 rax},
                  L.Instruction {src = [], dst = [rsp], val = MovImmediate 1 rsp},
                  L.Instruction {src = [], dst = [rbp], val = MovImmediate 1 rbp},
                  L.Instruction {src = [], dst = [rdi], val = MovImmediate 1 rdi},
                  L.Instruction {src = [], dst = [rsi], val = MovImmediate 1 rsi},
                  L.Instruction {src = [], dst = [rdx], val = MovImmediate 1 rdx},
                  L.Instruction {src = [], dst = [rcx], val = MovImmediate 1 rcx}
                ]
          allocateRegisters @CoalesceAllocation Procedure {body = body, frame = frame}
    result.body
      `shouldBe` [ MovImmediate 1 R8,
                   MovImmediate 1 R9,
                   MovImmediate 1 R10,
                   MovImmediate 1 R11,
                   MovImmediate 1 R12,
                   MovImmediate 1 R13,
                   MovImmediate 1 R14,
                   MovImmediate 1 R15,
                   MovImmediate 1 RIP,
                   MovImmediate 1 RAX,
                   MovImmediate 1 RSP,
                   MovImmediate 1 RBP,
                   MovImmediate 1 RDI,
                   MovImmediate 1 RSI,
                   MovImmediate 1 RDX,
                   MovImmediate 1 RCX
                 ]
