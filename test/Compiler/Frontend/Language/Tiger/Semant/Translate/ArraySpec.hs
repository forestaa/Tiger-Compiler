module Compiler.Frontend.Language.Tiger.Semant.Translate.ArraySpec (spec) where

import Compiler.Frontend.FrameMock (FrameMock)
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant (translateExp)
import Compiler.Frontend.Language.Tiger.Semant.Env (insertType)
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (allocateLocalVariableAndInsertType, isTypeCheckErrorAndExpectedArrayType, isTypeCheckErrorAndExpectedIntType, isTypeCheckErrorAndExpectedType, runEff)
import Compiler.Frontend.SrcLoc (RealLocated (L), dummyRealLocated)
import Compiler.Intermediate.Frame qualified as F (fp, wordSize)
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U (externalLabel, getUniqueEff)
import Compiler.Intermediate.Unique.TestUtils (newNthTemp, newNthUnique)
import RIO
import RIO.Text qualified as Text (unpack)
import Test.Hspec

spec :: Spec
spec = do
  translateArrayCreationSpec
  translateArrayIndexSpec

translateArrayCreationSpec :: Spec
translateArrayCreationSpec = describe "translate array creation test" $ do
  it "type array = array of int; array [0] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) (T.Int 1)
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "array" arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            initArray = U.externalLabel "initArray"
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Call (IR.Name initArray) [IR.Const 0, IR.Const 1])) (IR.Temp t))
        ty `shouldBe` TArray {id = id, range = TInt}

  it "type record = {}; type array = array of record; array [0] of nil" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) T.Nil
        result = runEff $ do
          id1 <- U.getUniqueEff #id
          id2 <- U.getUniqueEff #id
          let recordTy = TRecord [] id1
              arrayTy = TArray recordTy id2
          insertType "record" recordTy
          insertType "array" arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            initArray = U.externalLabel "initArray"
            id1 = newNthUnique 0
            id2 = newNthUnique 1
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Call (IR.Name initArray) [IR.Const 0, IR.Const 0])) (IR.Temp t))
        ty `shouldBe` TArray {id = id2, range = TRecord {id = id1, map = []}}

  it "type myint = int; myint [0] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "myint" (T.Int 0) (T.Int 1)
        result = runEff $ do
          insertType "myint" TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedArrayType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedArrayType

  it "type array = array of int; array [0] of 'hoge'" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) (T.String "hoge")
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "array" arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedType

  it "type array = array of int; array ['hoge'] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.String "hoge") (T.Int 0)
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "array" arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

translateArrayIndexSpec :: Spec
translateArrayIndexSpec = describe "translate array index test" $ do
  it "array index x[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x[1 + 1]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Op (T.Int 1) T.Plus (T.Int 1)))
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.BinOp IR.Plus (IR.Const 1) (IR.Const 1)) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x.y[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.RecField (T.Id "x") "y") (T.Int 0))
        result = runEff $ do
          id1 <- U.getUniqueEff #id
          id2 <- U.getUniqueEff #id
          let arrayTy = TArray TString id1
              recordTy = TRecord [("y", arrayTy)] id2
          allocateLocalVariableAndInsertType "x" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TString

  it "type synonym for array type" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
              nameTy = TName (dummyRealLocated "array")
          insertType "array" arrayTy
          allocateLocalVariableAndInsertType "x" True nameTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x['hoge']" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.String "hoge"))
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

  it "array type expected" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedArrayType" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedArrayType
