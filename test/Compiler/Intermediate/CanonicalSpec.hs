module Compiler.Intermediate.CanonicalSpec (spec) where

import Compiler.Frontend.FrameMock (isInRegister)
import Compiler.Frontend.FrameMock qualified as FM
import Compiler.Intermediate.Canonical (Block (..), basicBlocks, linearize, traceSchedule)
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR
import Compiler.Intermediate.Unique (newLabel)
import Compiler.Intermediate.Unique qualified as U (Label (..), Temp (..), Unique (..), evalUniqueEff)
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthNamedLabel, newNthTemp)
import Data.Extensible.Effect (leaveEff)
import RIO hiding (Const)
import RIO.Text (pack)
import Test.Hspec

spec :: Spec
spec = do
  linearizeSpec
  basicBlocksSpec
  traceScheduleSpec

linearizeSpec :: Spec
linearizeSpec = describe "linearize spec" $ do
  it "0 -> []" $ do
    let stm = Exp (Const 0)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body `shouldBe` []
    result.frame.numberOfLocals `shouldBe` 0

  it "noop is deleted" $ do
    let stm = Jump ((Exp (Const 0)) `ESeq` ((Exp (Const 0)) `ESeq` Const 0)) []
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body `shouldBe` [Jump (Const 0) []]
    result.frame.numberOfLocals `shouldBe` 0

  it "(s1, (s2, e)) -> (s1; s2, e)" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` (Move (Mem (Const 4)) (Const 2) `ESeq` BinOp Plus (Mem (Const 0)) (Mem (Const 4)))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Mem (Const 4)) (Const 2),
                   Exp (BinOp Plus (Mem (Const 0)) (Mem (Const 4)))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "(s, e1) + e2 -> (s, e1 + e2)" $ do
    let stm = Exp $ BinOp Plus (Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)) (Const 2)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (BinOp Plus (Mem (Const 0)) (Const 2))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "mem (s, e) -> (s, mem e)" $ do
    let stm = Exp $ Mem (Move (Mem (Const 0)) (Const 1) `ESeq` Const 4)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Mem (Const 4))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "jump (s, e) -> s; jump e" $ do
    let stm = Jump (Move (Mem (Const 0)) (Const 1) `ESeq` (Mem (Const 0))) []
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Jump (Mem (Const 0)) []
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "cjump ((s, e1) == e2) true false -> s; cjump (e1 == e2) true false" $ do
    let true = newNthNamedLabel "true" 0
        false = newNthNamedLabel "false" 1
        stm = CJump Eq (Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)) (Const 2) true false
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   CJump Eq (Mem (Const 0)) (Const 2) true false
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "e1 + (s, e2) -> (move temp e1, (s, temp + e2))" $ do
    let stm = Exp $ BinOp Plus (Mem (Const 0)) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp (newNthTemp 0)) (Mem (Const 0)),
                   Move (Mem (Const 0)) (Const 2),
                   Exp (BinOp Plus (Temp (newNthTemp 0)) (Mem (Const 0)))
                 ]
    result.frame.numberOfLocals `shouldBe` 1
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "e1 + (s, e2) -> (s, e1 + e2) where s and e1 are commutative" $ do
    let stm = Exp $ BinOp Plus (Const 1) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 2),
                   Exp (BinOp Plus (Const 1) (Mem (Const 0)))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "cjump (e1 == (s, e2)) true false -> move temp e1; s; cjump (temp == e2) true false" $ do
    let true = newNthNamedLabel "true" 0
        false = newNthNamedLabel "false" 1
        stm = CJump Eq (Mem (Const 0)) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0)) true false
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp (newNthTemp 0)) (Mem (Const 0)),
                   Move (Mem (Const 0)) (Const 2),
                   CJump Eq (Temp (newNthTemp 0)) (Mem (Const 0)) true false
                 ]
    result.frame.numberOfLocals `shouldBe` 1
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "cjump (e1 == (s, e2)) true false -> s; cjump (e1 == e2) true false where s and e1 are commutative" $ do
    let true = newNthNamedLabel "true" 0
        false = newNthNamedLabel "false" 1
        stm = CJump Eq (Const 1) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0)) true false
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 2),
                   CJump Eq (Const 1) (Mem (Const 0)) true false
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "f(e1) + g(e2) -> move temp1 f(e1); move temp2 g(e2); temp1 + temp2" $ do
    let t0 = newNthTemp 0
        t1 = newNthTemp 1
        t2 = newNthTemp 2
        f = newNthNamedLabel "f" 32
        g = newNthNamedLabel "g" 36
        stm = Exp $ BinOp Plus (Call (Name f) []) (Call (Name g) [])
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp t0) (Call (Name f) []),
                   Move (Temp t2) (Temp t0),
                   Move (Temp t1) (Call (Name g) []),
                   Exp (BinOp Plus (Temp t2) (Temp t1))
                 ]
    result.frame.numberOfLocals `shouldBe` 3
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "(s1; s2); (s3; s4) -> s1; s2; s3; s4" $ do
    let stm = (Move (Mem (Const 0)) (Const 1) `Seq` Move (Mem (Const 4)) (Const 2)) `Seq` (Move (Mem (Const 8)) (Const 3) `Seq` Move (Mem (Const 12)) (Const 4))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Mem (Const 4)) (Const 2),
                   Move (Mem (Const 8)) (Const 3),
                   Move (Mem (Const 12)) (Const 4)
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "move temp (s, e) -> s; move temp e" $ do
    let t32 = newNthTemp 32
        stm = Move (Temp t32) (Move (Temp t32) (Const 1) `ESeq` Temp t32)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp t32) (Const 1),
                   Move (Temp t32) (Temp t32)
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "move (mem (s, e1)) e2 -> s; move (mem e1) e2" $ do
    let stm = Move (Mem (Move (Mem (Const 4)) (Const 1) `ESeq` Const 0)) (Mem (Const 4))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 4)) (Const 1),
                   Move (Mem (Const 0)) (Mem (Const 4))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "(s, e) -> s; e" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Mem (Const 0))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "(s, f)(es) -> s; f(es)" $ do
    let f = newNthNamedLabel "f" 32
        stm = Exp $ Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name f) []
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Call (Name f) [])
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "move temp (s, f)(es) -> s; move temp f(es)" $ do
    let t32 = newNthTemp 32
        f = newNthNamedLabel "f" 36
        stm = Move (Temp t32) (Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name f) [])
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Temp t32) (Call (Name f) [])
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "f(e1, (s,e2), e3) -> f(move temp e1; s, temp), e2, e3)" $ do
    let t0 = newNthTemp 0
        f = newNthNamedLabel "f" 36
        stm = Exp $ Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name f) [Mem (Const 0), Move (Mem (Const 4)) (Const 2) `ESeq` Const 3, Const 4]
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Temp t0) (Mem (Const 0)),
                   Move (Mem (Const 4)) (Const 2),
                   Exp (Call (Name f) [Temp t0, Const 3, Const 4])
                 ]
    result.frame.numberOfLocals `shouldBe` 1
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "let var a: int = 1; var b: int = 2 in a + b" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` (Move (Mem (Const 4)) (Const 2) `ESeq` (BinOp Plus (Mem (Const 0)) (Mem (Const 4))))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (newNthLabel 0) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Mem (Const 4)) (Const 2),
                   Exp (BinOp Plus (Mem (Const 0)) (Mem (Const 4)))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

basicBlocksSpec :: Spec
basicBlocksSpec = describe "basicBlocks spec" $ do
  it "empty" $ do
    let stms = []
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          basicBlocks $ F.Procedure frame stms
    fst result.body `shouldBe` []

  it "1 block without Jump" $ do
    let label1 = newNthNamedLabel "l1" 10
        done = newNthLabel 1
        stms = [Label label1]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Jump (Name done) [done]]}
                 ]

  it "1 block with Jump" $ do
    let label1 = newNthNamedLabel "l1" 0
        stms = [Label label1, Exp (Const 0), Jump (Name label1) [label1]]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label1) [label1]]}
                 ]

  it "1 block with CJump" $ do
    let label1 = newNthNamedLabel "l1" 0
        label2 = newNthNamedLabel "l2" 1
        stms = [Label label1, Exp (Const 0), CJump Eq (Const 0) (Const 0) label1 label2]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Exp (Const 0), CJump Eq (Const 0) (Const 0) label1 label2]}
                 ]

  it "2 block with label followed by label" $ do
    let label1 = newNthNamedLabel "l1" 0
        label2 = newNthNamedLabel "l2" 1
        stms = [Label label1, Exp (Const 0), Label label2, CJump Eq (Const 0) (Const 0) label1 label2]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label2) [label2]]},
                   Block {lbl = label2, statements = [CJump Eq (Const 0) (Const 0) label1 label2]}
                 ]

  it "no label" $ do
    let label1 = newNthNamedLabel "l1" 0
        block = newNthLabel 1
        stms = [Exp (Const 0), Jump (Name label1) [label1]]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = block, statements = [Exp (Const 0), Jump (Name label1) [label1]]}
                 ]

traceScheduleSpec :: Spec
traceScheduleSpec = describe "traceSchedule spec" $ do
  it "0 block" $ do
    let done = newNthNamedLabel "done" 0
        blocks = []
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          traceSchedule $ F.Procedure frame (blocks, done)
    result.body `shouldBe` [Label done]

  it "2 block" $ do
    let label1 = newNthNamedLabel "l1" 10
        label2 = newNthNamedLabel "l2" 11
        done = newNthNamedLabel "done" 0
        false = newNthLabel 1
        blocks =
          [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label2) [label2]]},
            Block {lbl = label2, statements = [CJump Eq (Const 0) (Const 0) label1 label2]}
          ]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          traceSchedule $ F.Procedure frame (blocks, done)
    result.body
      `shouldBe` [ Label label1,
                   Exp (Const 0),
                   Label label2,
                   CJump Eq (Const 0) (Const 0) label1 false,
                   Label false,
                   Jump (Name label2) [label2],
                   Label done
                 ]

  it "5 block" $ do
    let label1 = newNthNamedLabel "l1" 10
        label2 = newNthNamedLabel "l2" 11
        label3 = newNthNamedLabel "l3" 12
        label4 = newNthNamedLabel "l4" 13
        label5 = newNthNamedLabel "l5" 14
        done = newNthNamedLabel "done" 15
        false = newNthLabel 1
        blocks =
          [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label2) [label2]]},
            Block {lbl = label2, statements = [CJump Eq (Const 0) (Const 0) label4 label3]},
            Block {lbl = label3, statements = [Exp (Const 0), Jump (Name done) [done]]},
            Block {lbl = label4, statements = [CJump Eq (Const 0) (Const 0) label2 label5]},
            Block {lbl = label5, statements = [CJump Eq (Const 0) (Const 0) label3 label2]}
          ]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          label <- newLabel
          frame <- FM.newFrame label []
          traceSchedule $ F.Procedure frame (blocks, done)
    result.body
      `shouldBe` [ Label label1,
                   Exp (Const 0),
                   Label label2,
                   CJump Eq (Const 0) (Const 0) label4 label3,
                   Label label3,
                   Exp (Const 0),
                   Jump (Name done) [done],
                   Label label4,
                   CJump Eq (Const 0) (Const 0) label2 label5,
                   Label label5,
                   CJump Eq (Const 0) (Const 0) label3 false,
                   Label false,
                   Jump (Name label2) [label2],
                   Label done
                 ]
