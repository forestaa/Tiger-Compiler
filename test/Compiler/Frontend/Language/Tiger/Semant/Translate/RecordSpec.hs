module Compiler.Frontend.Language.Tiger.Semant.Translate.RecordSpec (spec) where

import Compiler.Frontend.FrameMock (FrameMock)
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant (translateExp)
import Compiler.Frontend.Language.Tiger.Semant.Env (insertType)
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level (fetchCurrentLevelEff)
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (allocateLocalVariableAndInsertType, isTypeCheckErrorAndExpectedRecordType, isTypeCheckErrorAndExpectedTypeForRecordField, isTypeCheckErrorAndExtraRecordFieldInConstruction, isTypeCheckErrorAndMissingRecordField, isTypeCheckErrorAndMissingRecordFieldInConstruction, runEff)
import Compiler.Frontend.SrcLoc (RealLocated (L), dummyRealLocated)
import Compiler.Intermediate.Frame qualified as F (fp, wordSize)
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U (externalLabel, getUniqueEff)
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthTemp, newNthUnique)
import RIO
import RIO.Text qualified as Text (unpack)
import Test.Hspec

spec :: Spec
spec = do
  translateNilSpec
  translateRecordCreationSpec
  translateRecordFieldSpec

translateNilSpec :: Spec
translateNilSpec = describe "translate nil test" $ do
  it "translate nil" $ do
    let ast = T.expToLExp T.Nil
    case runEff (translateExp ast) of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((_, ty), _), _) -> do
        ty `shouldBe` TNil

translateRecordCreationSpec :: Spec
translateRecordCreationSpec = describe "translate record creation test" $ do
  it "type record = {}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = U.externalLabel "malloc"
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const 0])) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = []}

  it "type record = {x: int}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = U.externalLabel "malloc"
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (F.wordSize @FrameMock)])) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 1))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("x", TInt)]}

  it "type record = {x: int, y: string}; record {x = 1, y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1), T.FieldAssign "y" (T.String "hoge")]
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt), ("y", TString)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = U.externalLabel "malloc"
            l = newNthLabel 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (2 * (F.wordSize @FrameMock))])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 1)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const (F.wordSize @FrameMock)))) (IR.Name l)))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("x", TInt), ("y", TString)]}

  it "type record = {y: int, x: string}; record {y = 1, x = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "y" (T.Int 1), T.FieldAssign "x" (T.String "hoge")]
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("y", TInt), ("x", TString)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = U.externalLabel "malloc"
            l = newNthLabel 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (2 * (F.wordSize @FrameMock))])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 1)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const (F.wordSize @FrameMock)))) (IR.Name l)))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("y", TInt), ("x", TString)]}

  it "type record1 = {}; type record2 = {x: record1};  record2 {x: nil}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record2" [T.FieldAssign "x" T.Nil]
        result = runEff $ do
          id1 <- U.getUniqueEff #id
          id2 <- U.getUniqueEff #id
          let record1Ty = TRecord [] id1
              record2Ty = TRecord [("x", record1Ty)] id2
          insertType "record1" record1Ty
          insertType "record2" record2Ty
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = U.externalLabel "malloc"
            id1 = newNthUnique 0
            id2 = newNthUnique 1
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (F.wordSize @FrameMock)])) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 0))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id2, map = [("x", TRecord {id = id1, map = []})]}

  it "type record = {x: int}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMissingRecordFieldInConstruction

  it "type record = {}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExtraRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExtraRecordFieldInConstruction

  it "type record = {x: int}; record {y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "y" (T.String "hoge")]
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMissingRecordFieldInConstruction

  it "type record = {x: int}; record {x = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.String "hoge")]
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeForRecordField: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedTypeForRecordField

  it "type myint = int; myint {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "myint" []
        result = runEff $ do
          insertType "myint" TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedRecordType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedRecordType

translateRecordFieldSpec :: Spec
translateRecordFieldSpec = describe "translate record field test" $ do
  it "first record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          allocateLocalVariableAndInsertType "object" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)))
        ty `shouldBe` TInt

  it "second record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "y")
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt), ("y", TString)] id
          allocateLocalVariableAndInsertType "object" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const (F.wordSize @FrameMock))))
        ty `shouldBe` TString

  it "type synonym for record type" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
              nameTy = TName (dummyRealLocated "record")
          insertType "record" recordTy
          allocateLocalVariableAndInsertType "object" True nameTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)))
        ty `shouldBe` TInt

  it "not record type" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          allocateLocalVariableAndInsertType "object" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedRecordType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedRecordType

  it "missing record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "z")
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt), ("y", TString)] id
          allocateLocalVariableAndInsertType "object" True recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordField: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMissingRecordField
