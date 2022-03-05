module CanonicalSpec (spec) where

import Canonical (linearize)
import Data.Extensible.Effect (leaveEff)
import IR
import RIO hiding (Const)
import RIO.Text (pack)
import Test.Hspec
import Unique qualified as U (Label (..), Temp (..), Unique (..), runUniqueEff)

spec :: Spec
spec = do
  linearizeSpec

linearizeSpec :: Spec
linearizeSpec = describe "linearize spec" $ do
  it "0 -> []" $ do
    let stm = Exp (Const 0)
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result `shouldBe` []

  it "noop is deleted" $ do
    let stm = Jump ((Exp (Const 0)) `ESeq` ((Exp (Const 0)) `ESeq` Const 0)) []
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result `shouldBe` [Jump (Const 0) []]

  it "(s1, (s2, e)) -> (s1; s2, e)" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` (Move (Mem (Const 4)) (Const 2) `ESeq` BinOp Plus (Mem (Const 0)) (Mem (Const 4)))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Mem (Const 4)) (Const 2),
                   Exp (BinOp Plus (Mem (Const 0)) (Mem (Const 4)))
                 ]

  it "(s, e1) + e2 -> (s, e1 + e2)" $ do
    let stm = Exp $ BinOp Plus (Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)) (Const 2)
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (BinOp Plus (Mem (Const 0)) (Const 2))
                 ]

  it "mem (s, e) -> (s, mem e)" $ do
    let stm = Exp $ Mem (Move (Mem (Const 0)) (Const 1) `ESeq` Const 4)
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Mem (Const 4))
                 ]

  it "jump (s, e) -> s; jump e" $ do
    let stm = Jump (Move (Mem (Const 0)) (Const 1) `ESeq` (Mem (Const 0))) []
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Jump (Mem (Const 0)) []
                 ]

  it "cjump ((s, e1) == e2) true false -> s; cjump (e1 == e2) true false" $ do
    let stm = CJump Eq (Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)) (Const 2) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   CJump Eq (Mem (Const 0)) (Const 2) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
                 ]

  it "e1 + (s, e2) -> (move temp e1, (s, temp + e2))" $ do
    let stm = Exp $ BinOp Plus (Mem (Const 0)) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Temp (U.Temp (U.Unique 0))) (Mem (Const 0)),
                   Move (Mem (Const 0)) (Const 2),
                   Exp (BinOp Plus (Temp (U.Temp (U.Unique 0))) (Mem (Const 0)))
                 ]

  it "e1 + (s, e2) -> (s, e1 + e2) where s and e1 are commutative" $ do
    let stm = Exp $ BinOp Plus (Const 1) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 2),
                   Exp (BinOp Plus (Const 1) (Mem (Const 0)))
                 ]

  it "cjump (e1 == (s, e2)) true false -> move temp e1; s; cjump (temp == e2) true false" $ do
    let stm = CJump Eq (Mem (Const 0)) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Temp (U.Temp (U.Unique 0))) (Mem (Const 0)),
                   Move (Mem (Const 0)) (Const 2),
                   CJump Eq (Temp (U.Temp (U.Unique 0))) (Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
                 ]

  it "cjump (e1 == (s, e2)) true false -> s; cjump (e1 == e2) true false where s and e1 are commutative" $ do
    let stm = CJump Eq (Const 1) (Move (Mem (Const 0)) (Const 2) `ESeq` Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 2),
                   CJump Eq (Const 1) (Mem (Const 0)) (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
                 ]

  it "f(e1) + g(e2) -> move temp1 f(e1); move temp2 g(e2); temp1 + temp2" $ do
    let stm = Exp $ BinOp Plus (Call (Name (U.Label "f" (U.Unique 32))) []) (Call (Name (U.Label "g" (U.Unique 36))) [])
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Temp (U.Temp (U.Unique 0))) (Call (Name (U.Label "f" (U.Unique 32))) []),
                   Move (Temp (U.Temp (U.Unique 2))) (Temp (U.Temp (U.Unique 0))),
                   Move (Temp (U.Temp (U.Unique 1))) (Call (Name (U.Label "g" (U.Unique 36))) []),
                   Exp (BinOp Plus (Temp (U.Temp (U.Unique 2))) (Temp (U.Temp (U.Unique 1))))
                 ]

  it "(s1; s2); (s3; s4) -> s1; s2; s3; s4" $ do
    let stm = (Move (Mem (Const 0)) (Const 1) `Seq` Move (Mem (Const 4)) (Const 2)) `Seq` (Move (Mem (Const 8)) (Const 3) `Seq` Move (Mem (Const 12)) (Const 4))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Mem (Const 4)) (Const 2),
                   Move (Mem (Const 8)) (Const 3),
                   Move (Mem (Const 12)) (Const 4)
                 ]

  it "move temp (s, e) -> s; move temp e" $ do
    let stm = Move (Temp (U.Temp (U.Unique 32))) (Move (Temp (U.Temp (U.Unique 32))) (Const 1) `ESeq` Temp (U.Temp (U.Unique 32)))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Temp (U.Temp (U.Unique 32))) (Const 1),
                   Move (Temp (U.Temp (U.Unique 32))) (Temp (U.Temp (U.Unique 32)))
                 ]

  it "move (mem (s, e1)) e2 -> s; move (mem e1) e2" $ do
    let stm = Move (Mem (Move (Mem (Const 4)) (Const 1) `ESeq` Const 0)) (Mem (Const 4))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 4)) (Const 1),
                   Move (Mem (Const 0)) (Mem (Const 4))
                 ]

  it "(s, e) -> s; e" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` Mem (Const 0)
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Mem (Const 0))
                 ]

  it "(s, f)(es) -> s; f(es)" $ do
    let stm = Exp $ Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name (U.Label "f" (U.Unique 32))) []
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Exp (Call (Name (U.Label "f" (U.Unique 32))) [])
                 ]

  it "move temp (s, f)(es) -> s; move temp f(es)" $ do
    let stm = Move (Temp (U.Temp (U.Unique 32))) (Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name (U.Label "f" (U.Unique 36))) [])
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Temp (U.Temp (U.Unique 32))) (Call (Name (U.Label "f" (U.Unique 36))) [])
                 ]

  it "f(e1, (s,e2), e3) -> f(move temp e1; s, temp), e2, e3)" $ do
    let stm = Exp $ Call (Move (Mem (Const 0)) (Const 1) `ESeq` Name (U.Label "f" (U.Unique 36))) [Mem (Const 0), Move (Mem (Const 4)) (Const 2) `ESeq` Const 3, Const 4]
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Temp (U.Temp (U.Unique 0))) (Mem (Const 0)),
                   Move (Mem (Const 4)) (Const 2),
                   Exp (Call (Name (U.Label "f" (U.Unique 36))) [Temp (U.Temp (U.Unique 0)), Const 3, Const 4])
                 ]

  it "let var a: int = 1; var b: int = 2 in a + b" $ do
    let stm = Exp $ Move (Mem (Const 0)) (Const 1) `ESeq` (Move (Mem (Const 4)) (Const 2) `ESeq` (BinOp Plus (Mem (Const 0)) (Mem (Const 4))))
        result = leaveEff . U.runUniqueEff @"temp" $ linearize stm
    result
      `shouldBe` [ Move (Mem (Const 0)) (Const 1),
                   Move (Mem (Const 4)) (Const 2),
                   Exp (BinOp Plus (Mem (Const 0)) (Mem (Const 4)))
                 ]
