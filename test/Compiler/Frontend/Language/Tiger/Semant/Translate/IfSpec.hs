module Compiler.Frontend.Language.Tiger.Semant.Translate.IfSpec (spec) where

import Compiler.Frontend.FrameMock (FrameMock)
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant (translateExp)
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level (fetchCurrentLevelEff)
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (allocateLocalVariableAndInsertType, insertFun, isTypeCheckErrorAndExpectedIntType, isTypeCheckErrorAndExpectedUnitType, runEff)
import Compiler.Frontend.SrcLoc (RealLocated (L), dummyRealLocated)
import Compiler.Intermediate.Frame qualified as F (fp, wordSize)
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U (getUniqueEff, newLabel)
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthTemp)
import RIO
import RIO.Text qualified as Text (unpack)
import Test.Hspec

spec :: Spec
spec = do
  translateIfElseSpec
  translateIfNoElseSpec

translateIfElseSpec :: Spec
translateIfElseSpec = describe "translate if-else test" $ do
  it "if 0 == 0 then 1 else 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 1) (Just (T.Int 0))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
            z = newNthLabel 3
            t0 = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Temp t0) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label false) (IR.Seq (IR.Move (IR.Temp t0) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Label z)))))))) (IR.Temp t0))
        ty `shouldBe` TInt

  it "if 0 == 0 then x := 0 else x := 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0)) (Just (T.Assign (T.Id "x") (T.Int 1)))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
            z = newNthLabel 3
            t = newNthTemp 0
        exp `shouldBe` Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label false) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Label z))))))))
        ty `shouldBe` TUnit

  it "if 0 == 0 then 0 == 0 else 0 == 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Op (T.Int 0) T.Eq (T.Int 0)) (Just (T.Op (T.Int 0) T.Eq (T.Int 1)))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            true = newNthLabel 1
            false = newNthLabel 2
            true' = newNthLabel 3
            false' = newNthLabel 4
            z = newNthLabel 5
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true' false') (IR.Seq (IR.Label false) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 1) true' false') (IR.Seq (IR.Label true') (IR.Seq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label false') (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Label z)))))))))))) (IR.Temp t))
        ty `shouldBe` TInt

  it "if 0 == 0 then 0 else 0 == 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 0) (Just (T.Op (T.Int 0) T.Eq (T.Int 1)))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
            z = newNthLabel 3
            true' = newNthLabel 4
            false' = newNthLabel 5
            t0 = newNthTemp 0
            t1 = newNthTemp 1
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Temp t0) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label false) (IR.Seq (IR.Move (IR.Temp t0) (IR.ESeq (IR.Seq (IR.Move (IR.Temp t1) (IR.Const 1)) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 1) true' false') (IR.Seq (IR.Label false') (IR.Seq (IR.Move (IR.Temp t1) (IR.Const 0)) (IR.Label true'))))) (IR.Temp t1))) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Label z)))))))) (IR.Temp t0))
        ty `shouldBe` TInt

  it "if 0 then 0 else 1" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Int 0) (Just (T.Int 1))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
            z = newNthLabel 3
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Jump (IR.Name false) [false]) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label false) (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Label z)))))))) (IR.Temp t))
        ty `shouldBe` TInt

  it "if 1 + 1 then 0 else 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 1) T.Plus (T.Int 1)) (T.Int 0) (Just (T.Int 1))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
            z = newNthLabel 3
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.CJump IR.Ne (IR.BinOp IR.Plus (IR.Const 1) (IR.Const 1)) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label false) (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Label z)))))))) (IR.Temp t))
        ty `shouldBe` TInt

  it "if x(array) then 0 else 1" $ do
    let ast = T.expToLExp $ T.If (T.Var (T.Id "x")) (T.Assign (T.Id "x") (T.Int 0)) (Just (T.Assign (T.Id "x") (T.Int 1)))
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

translateIfNoElseSpec :: Spec
translateIfNoElseSpec = describe "translate if-no-else test" $ do
  it "if 0 == 0 then x := 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
        exp `shouldBe` Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)) (IR.Label false))))
        ty `shouldBe` TUnit

  it "if 0 then x := 0" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let z = newNthLabel 2
            z' = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label z') (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)) (IR.Label z))))
        ty `shouldBe` TUnit

  it "f: () -> (); if 0 == 0 then f()" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.FunApply "f" []) Nothing
        result = runEff $ do
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 2
            false = newNthLabel 3
            f = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Exp (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock)])) (IR.Label false))))
        ty `shouldBe` TUnit

  it "if x(array) then 0" $ do
    let ast = T.expToLExp $ T.If (T.Var (T.Id "x")) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

  it "if 0 then 0" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Int 0) Nothing
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedUnitType
