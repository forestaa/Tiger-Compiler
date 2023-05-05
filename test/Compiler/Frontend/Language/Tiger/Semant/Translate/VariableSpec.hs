module Compiler.Frontend.Language.Tiger.Semant.Translate.VariableSpec where

import Compiler.Frontend.FrameMock (FrameMock)
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant (translateExp)
import Compiler.Frontend.Language.Tiger.Semant.Env (VarAccess (FunAccess), VarType (FunType), insertVarAccess, insertVarType)
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (allocateLocalVariableAndInsertType, isTypeCheckErrorAndExpectedType, isTypeCheckErrorAndExpectedVariable, isTypeCheckErrorAndUndefinedVariable, runEff)
import Compiler.Frontend.SrcLoc (RealLocated (L))
import Compiler.Intermediate.Frame qualified as F (fp, wordSize)
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthTemp)
import RIO
import RIO.Text qualified as Text (unpack)
import Test.Hspec

spec :: Spec
spec = do
  translateVariableSpec
  translateAssignSpec

translateVariableSpec :: Spec
translateVariableSpec = describe "translate variable test" $ do
  it "first local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          allocateLocalVariableAndInsertType "y" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-2 * F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable, first is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          allocateLocalVariableAndInsertType "y" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "first local variable, second is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          allocateLocalVariableAndInsertType "y" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "local variable, not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Temp (newNthTemp 0))
        ty `shouldBe` TInt

  it "undefined variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUndefinedVariable

  it "variable referes function" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          insertVarType "x" $ FunType undefined undefined
          insertVarAccess "x" $ FunAccess undefined undefined
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedVariable

translateAssignSpec :: Spec
translateAssignSpec = describe "translate assgin test" $ do
  it "var x int; x := 0" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "x") (T.Int 0)
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
        exp `shouldBe` Nx (IR.Move (IR.Temp t) (IR.Const 0))
        ty `shouldBe` TUnit

  it "var x int; var y: unit; y := (x := 0)" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "y") (T.Assign (T.Id "x") (T.Int 0))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          allocateLocalVariableAndInsertType "y" False TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t0 = newNthTemp 0
            t1 = newNthTemp 1
        exp `shouldBe` Nx (IR.Move (IR.Temp t1) (IR.ESeq (IR.Move (IR.Temp t0) (IR.Const 0)) (IR.Const 0)))
        ty `shouldBe` TUnit

  it "var x string; x := 0" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "x") (T.Int 0)
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TString
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedType
