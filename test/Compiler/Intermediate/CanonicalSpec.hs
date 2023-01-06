module Compiler.Intermediate.CanonicalSpec (spec) where

import Compiler.Frontend.FrameMock (isInRegister)
import Compiler.Frontend.FrameMock qualified as FM
import Compiler.Intermediate.Canonical (Block (..), basicBlocks, linearize, traceSchedule)
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR
import Compiler.Intermediate.Unique qualified as U (Label (..), Temp (..), Unique (..), evalUniqueEff)
import Data.Extensible.Effect (leaveEff)
import RIO hiding (Const)
import RIO.Text (pack)
import Test.Hspec

spec :: Spec
spec = do
  linearizeSpec

-- basicBlocksSpec
-- traceScheduleSpec

linearizeSpec :: Spec
linearizeSpec = describe "linearize spec" $ do
  it "0 -> []" $ do
    let stm = Exp (Const 0)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body `shouldBe` []
    result.frame.numberOfLocals `shouldBe` 0

  it "noop is deleted" $ do
    let stm = Jump ((Exp (Const 0)) `ESeq` ((Exp (Const 0)) `ESeq` Const 0)) []
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body `shouldBe` [Jump (Const 0) []]
    result.frame.numberOfLocals `shouldBe` 0

  it "(s1, (s2, e)) -> (s1; s2, e)" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` (Move (Mem (Const 4)) (Const 2) `ESeq` BinOp Plus (Mem (Const 0)) (Mem (Const 4)))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
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
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (BinOp Plus (Mem (Const 0)) (Const 2))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "mem (s, e) -> (s, mem e)" $ do
    let stm = Exp $ Mem (Move (Mem (Const 0)) (Const 1) `ESeq` Const 4)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Mem (Const 4))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "jump (s, e) -> s; jump e" $ do
    let stm = Jump (Move (Mem (Const 0)) (Const 1) `ESeq` (Mem (Const 0))) []
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Jump (Mem (Const 0)) []
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "cjump ((s, e1) == e2) true false -> s; cjump (e1 == e2) true false" $ do
    let stm = CJump Eq (Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)) (Const 2) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   CJump Eq (Mem (Const 0)) (Const 2) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "e1 + (s, e2) -> (move temp e1, (s, temp + e2))" $ do
    let stm = Exp $ BinOp Plus (Mem (Const 0)) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp (U.Temp "t" (U.Unique 0))) (Mem (Const 0)),
                   Move (Mem (Const 0)) (Const 2),
                   Exp (BinOp Plus (Temp (U.Temp "t" (U.Unique 0))) (Mem (Const 0)))
                 ]
    result.frame.numberOfLocals `shouldBe` 1
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "e1 + (s, e2) -> (s, e1 + e2) where s and e1 are commutative" $ do
    let stm = Exp $ BinOp Plus (Const 1) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 2),
                   Exp (BinOp Plus (Const 1) (Mem (Const 0)))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "cjump (e1 == (s, e2)) true false -> move temp e1; s; cjump (temp == e2) true false" $ do
    let stm = CJump Eq (Mem (Const 0)) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp (U.Temp "t" (U.Unique 0))) (Mem (Const 0)),
                   Move (Mem (Const 0)) (Const 2),
                   CJump Eq (Temp (U.Temp "t" (U.Unique 0))) (Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
                 ]
    result.frame.numberOfLocals `shouldBe` 1
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "cjump (e1 == (s, e2)) true false -> s; cjump (e1 == e2) true false where s and e1 are commutative" $ do
    let stm = CJump Eq (Const 1) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 2),
                   CJump Eq (Const 1) (Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "f(e1) + g(e2) -> move temp1 f(e1); move temp2 g(e2); temp1 + temp2" $ do
    let stm = Exp $ BinOp Plus (Call (Name (U.Label "f" (U.Unique 32))) []) (Call (Name (U.Label "g" (U.Unique 36))) [])
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp (U.Temp "t" (U.Unique 0))) (Call (Name (U.Label "f" (U.Unique 32))) []),
                   Move (Temp (U.Temp "t" (U.Unique 2))) (Temp (U.Temp "t" (U.Unique 0))),
                   Move (Temp (U.Temp "t" (U.Unique 1))) (Call (Name (U.Label "g" (U.Unique 36))) []),
                   Exp (BinOp Plus (Temp (U.Temp "t" (U.Unique 2))) (Temp (U.Temp "t" (U.Unique 1))))
                 ]
    result.frame.numberOfLocals `shouldBe` 3
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "(s1; s2); (s3; s4) -> s1; s2; s3; s4" $ do
    let stm = (Move (Mem (Const 0)) (Const 1) `Seq` Move (Mem (Const 4)) (Const 2)) `Seq` (Move (Mem (Const 8)) (Const 3) `Seq` Move (Mem (Const 12)) (Const 4))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Mem (Const 4)) (Const 2),
                   Move (Mem (Const 8)) (Const 3),
                   Move (Mem (Const 12)) (Const 4)
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "move temp (s, e) -> s; move temp e" $ do
    let stm = Move (Temp (U.Temp "t" (U.Unique 32))) (Move (Temp (U.Temp "t" (U.Unique 32))) (Const 1) `ESeq` Temp (U.Temp "t" (U.Unique 32)))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Temp (U.Temp "t" (U.Unique 32))) (Const 1),
                   Move (Temp (U.Temp "t" (U.Unique 32))) (Temp (U.Temp "t" (U.Unique 32)))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "move (mem (s, e1)) e2 -> s; move (mem e1) e2" $ do
    let stm = Move (Mem (Move (Mem (Const 4)) (Const 1) `ESeq` Const 0)) (Mem (Const 4))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 4)) (Const 1),
                   Move (Mem (Const 0)) (Mem (Const 4))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "(s, e) -> s; e" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Mem (Const 0))
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "(s, f)(es) -> s; f(es)" $ do
    let stm = Exp $ Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name (U.Label "f" (U.Unique 32))) []
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Call (Name (U.Label "f" (U.Unique 32))) [])
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "move temp (s, f)(es) -> s; move temp f(es)" $ do
    let stm = Move (Temp (U.Temp "t" (U.Unique 32))) (Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name (U.Label "f" (U.Unique 36))) [])
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Temp (U.Temp "t" (U.Unique 32))) (Call (Name (U.Label "f" (U.Unique 36))) [])
                 ]
    result.frame.numberOfLocals `shouldBe` 0

  it "f(e1, (s,e2), e3) -> f(move temp e1; s, temp), e2, e3)" $ do
    let stm = Exp $ Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name (U.Label "f" (U.Unique 36))) [Mem (Const 0), Move (Mem (Const 4)) (Const 2) `ESeq` Const 3, Const 4]
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          linearize $ F.Procedure frame stm
    result.body
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Temp (U.Temp "t" (U.Unique 0))) (Mem (Const 0)),
                   Move (Mem (Const 4)) (Const 2),
                   Exp (Call (Name (U.Label "f" (U.Unique 36))) [Temp (U.Temp "t" (U.Unique 0)), Const 3, Const 4])
                 ]
    result.frame.numberOfLocals `shouldBe` 1
    result.frame.localVariables `shouldSatisfy` all isInRegister

  it "let var a: int = 1; var b: int = 2 in a + b" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` (Move (Mem (Const 4)) (Const 2) `ESeq` (BinOp Plus (Mem (Const 0)) (Mem (Const 4))))
        result = leaveEff . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
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
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          basicBlocks $ F.Procedure frame stms
    fst result.body `shouldBe` []

  it "1 block without Jump" $ do
    let label1 = U.Label "l1" (U.Unique 0)
        stms = [Label label1]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Jump (Name (U.Label "L" (U.Unique 0))) [U.Label "L" (U.Unique 0)]]}
                 ]

  it "1 block with Jump" $ do
    let label1 = U.Label "l1" (U.Unique 0)
        stms = [Label label1, Exp (Const 0), Jump (Name label1) [label1]]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label1) [label1]]}
                 ]

  it "1 block with CJump" $ do
    let label1 = U.Label "l1" (U.Unique 0)
        label2 = U.Label "l2" (U.Unique 1)
        stms = [Label label1, Exp (Const 0), CJump Eq (Const 0) (Const 0) label1 label2]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Exp (Const 0), CJump Eq (Const 0) (Const 0) label1 label2]}
                 ]

  it "2 block with label followed by label" $ do
    let label1 = U.Label "l1" (U.Unique 0)
        label2 = U.Label "l2" (U.Unique 1)
        stms = [Label label1, Exp (Const 0), Label label2, CJump Eq (Const 0) (Const 0) label1 label2]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label2) [label2]]},
                   Block {lbl = label2, statements = [CJump Eq (Const 0) (Const 0) label1 label2]}
                 ]

  it "no label" $ do
    let label1 = U.Label "l1" (U.Unique 0)
        stms = [Exp (Const 0), Jump (Name label1) [label1]]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          basicBlocks $ F.Procedure frame stms
    fst result.body
      `shouldBe` [ Block {lbl = U.Label "L" (U.Unique 0), statements = [Exp (Const 0), Jump (Name label1) [label1]]}
                 ]

traceScheduleSpec :: Spec
traceScheduleSpec = describe "traceSchedule spec" $ do
  it "0 block" $ do
    let done = U.Label "done" (U.Unique 0)
        blocks = []
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          traceSchedule $ F.Procedure frame (blocks, done)
    result.body `shouldBe` [Label done]

  it "2 block" $ do
    let label1 = U.Label "l1" (U.Unique 10)
        label2 = U.Label "l2" (U.Unique 11)
        done = U.Label "done" (U.Unique 0)
        blocks =
          [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label2) [label2]]},
            Block {lbl = label2, statements = [CJump Eq (Const 0) (Const 0) label1 label2]}
          ]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
          traceSchedule $ F.Procedure frame (blocks, done)
    result.body
      `shouldBe` [ Label label1,
                   Exp (Const 0),
                   Label label2,
                   CJump Eq (Const 0) (Const 0) label1 (U.Label "L" (U.Unique 0)),
                   Label (U.Label "L" (U.Unique 0)),
                   Jump (Name label2) [label2],
                   Label done
                 ]

  it "5 block" $ do
    let label1 = U.Label "l1" (U.Unique 10)
        label2 = U.Label "l2" (U.Unique 11)
        label3 = U.Label "l3" (U.Unique 12)
        label4 = U.Label "l4" (U.Unique 13)
        label5 = U.Label "l5" (U.Unique 14)
        done = U.Label "done" (U.Unique 15)
        blocks =
          [ Block {lbl = label1, statements = [Exp (Const 0), Jump (Name label2) [label2]]},
            Block {lbl = label2, statements = [CJump Eq (Const 0) (Const 0) label4 label3]},
            Block {lbl = label3, statements = [Exp (Const 0), Jump (Name done) [done]]},
            Block {lbl = label4, statements = [CJump Eq (Const 0) (Const 0) label2 label5]},
            Block {lbl = label5, statements = [CJump Eq (Const 0) (Const 0) label3 label2]}
          ]
        result = leaveEff . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
          frame <- FM.newFrame (U.Label "" (U.Unique 0)) []
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
                   CJump Eq (Const 0) (Const 0) label3 (U.Label "L" (U.Unique 0)),
                   Label (U.Label "L" (U.Unique 0)),
                   Jump (Name label2) [label2],
                   Label done
                 ]
