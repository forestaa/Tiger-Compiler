module Compiler.Frontend.Language.Tiger.SemantSpec (spec) where

import Compiler.Frontend.FrameMock
import Compiler.Frontend.Id
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant
import Compiler.Frontend.Language.Tiger.Semant.BreakPoint
import Compiler.Frontend.Language.Tiger.Semant.Env
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level
import Compiler.Frontend.Language.Tiger.Semant.Translate
import Compiler.Frontend.Language.Tiger.Semant.TypeCheck
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils
import Compiler.Frontend.SrcLoc
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthNamedLabel, newNthTemp, newNthUnique)
import Compiler.Utils.Maybe
import Data.Extensible
import Data.Extensible.Effect
import RIO
import RIO.List qualified as List
import RIO.List.Partial qualified as Partial
import RIO.Text qualified as T
import Test.Hspec

spec :: Spec
spec = do
  translateIntSpec
  translateStringSpec
  translateNilSpec
  translateVariableSpec
  translateRecordFieldSpec
  translateArrayIndexSpec
  translateBinOpSpec
  translateIfElseSpec
  translateIfNoElseSpec
  translateRecordCreationSpec
  translateArrayCreationSpec
  translateWhileLoopSpec
  translateForLoopSpec
  translateBreakSpec
  translateFunApplySpec
  translateAssignSpec
  translateSeqSpec
  translateLetSpec

translateIntSpec :: Spec
translateIntSpec = describe "translate int test" $ do
  it "translate 0" $ do
    let ast = T.expToLExp $ T.Int 0
    case runEff (translateExp ast) of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt

translateStringSpec :: Spec
translateStringSpec = describe "translate string test" $ do
  it "translate 'hoge'" $ do
    let ast = T.expToLExp $ T.String "hoge"
    case runEff (translateExp ast) of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Ex (IR.Name (newNthLabel 1))
        ty `shouldBe` TString
        length fragments.fragments `shouldBe` 1
        (Partial.head fragments.fragments).string.text `shouldBe` Just "hoge"

translateNilSpec :: Spec
translateNilSpec = describe "translate nil test" $ do
  it "translate nil" $ do
    let ast = T.expToLExp $ T.Nil
    case runEff (translateExp ast) of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((_, ty), _), _) -> do
        ty `shouldBe` TNil

translateVariableSpec :: Spec
translateVariableSpec = describe "translate variable test" $ do
  it "first local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "local variable, not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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

translateRecordFieldSpec :: Spec
translateRecordFieldSpec = describe "translate record field test" $ do
  it "first record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          allocateLocalVariableAndInsertType "object" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)))
        ty `shouldBe` TInt

  it "second record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "y")
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt), ("y", TString)] id
          allocateLocalVariableAndInsertType "object" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const (F.wordSize @FrameMock))))
        ty `shouldBe` TString

  it "type synonym for record type" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
              nameTy = TName (dummyRealLocated "record")
          insertType "record" recordTy
          allocateLocalVariableAndInsertType "object" True nameTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt), ("y", TString)] id
          allocateLocalVariableAndInsertType "object" True recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordField: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMissingRecordField

translateArrayIndexSpec :: Spec
translateArrayIndexSpec = describe "translate array index test" $ do
  it "array index x[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x[1 + 1]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Op (T.Int 1) T.Plus (T.Int 1)))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.BinOp IR.Plus (IR.Const 1) (IR.Const 1)) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x.y[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.RecField (T.Id "x") "y") (T.Int 0))
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let arrayTy = TArray TString id1
              recordTy = TRecord [("y", arrayTy)] id2
          allocateLocalVariableAndInsertType "x" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TString

  it "type synonym for array type" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
              nameTy = TName (dummyRealLocated "array")
          insertType "array" arrayTy
          allocateLocalVariableAndInsertType "x" True nameTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x['hoge']" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.String "hoge"))
        result = runEff $ do
          id <- getUniqueEff #id
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

translateBinOpSpec :: Spec
translateBinOpSpec = describe "translate binop test" $ do
  it "0 + 0" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.BinOp IR.Plus (IR.Const 0) (IR.Const 0))
        ty `shouldBe` TInt

  it "'hoge' + 1" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Plus (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

  it "x + x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "x"))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

  it "x == x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "x"))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((Cx genstm, ty), _), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "nil ≠ nil" $ do
    let ast = T.expToLExp $ T.Op T.Nil T.NEq T.Nil
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return NotdeterminedNilType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndNotDeterminedNilType

  it "nil == x (record)" $ do
    let ast = T.expToLExp $ T.Op T.Nil T.Eq (T.Var (T.Id "x"))
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          allocateLocalVariableAndInsertType "x" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((Cx genstm, ty), _), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Const 0) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "'hoge' == 'hoge'" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Eq (T.String "hoge")
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((Cx genstm, ty), _), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Ne (IR.Call (IR.Name (newNthNamedLabel "stringEqual" 3)) [IR.Name (newNthLabel 1), IR.Name (newNthLabel 2)]) (IR.Const 0) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "'hoge' <> 'hoge'" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.NEq (T.String "hoge")
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((Cx genstm, ty), _), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Call (IR.Name (newNthNamedLabel "stringEqual" 3)) [IR.Name (newNthLabel 1), IR.Name (newNthLabel 2)]) (IR.Const 0) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "x == y (array == record)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "y"))
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id1
              arrayTy = TArray TInt id2
          allocateLocalVariableAndInsertType "x" True recordTy
          allocateLocalVariableAndInsertType "y" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedType

  it "0 + (0 == 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Op (T.Int 0) T.Eq (T.Int 0))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
        exp `shouldBe` Ex (IR.BinOp IR.Plus (IR.Const 0) (IR.ESeq (IR.Seq (IR.Move (IR.Temp (newNthTemp 0)) (IR.Const 1)) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label false) (IR.Seq (IR.Move (IR.Temp (newNthTemp 0)) (IR.Const 0)) (IR.Label true))))) (IR.Temp (newNthTemp 0))))
        ty `shouldBe` TInt

  it "0 == (0 == 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Eq (T.Op (T.Int 0) T.Eq (T.Int 0))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((Cx genstm, ty), _), _) -> do
        let (true, false) = fetchTwoLabel
            t0 = newNthTemp 0
            l2 = newNthLabel 2
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Const 0) (IR.ESeq (IR.Seq (IR.Move (IR.Temp t0) (IR.Const 1)) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) false l2) (IR.Seq (IR.Label l2) (IR.Seq (IR.Move (IR.Temp t0) (IR.Const 0)) (IR.Label false))))) (IR.Temp t0)) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "(x := 0) == (x := 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Assign (T.Id "x") (T.Int 0)) T.Eq (T.Assign (T.Id "x") (T.Int 0))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedExpression: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedExpression

  it "'hoge' < 'hoge'" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Lt (T.String "hoge")
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

translateIfElseSpec :: Spec
translateIfElseSpec = describe "translate if-else test" $ do
  it "if 0 == 0 then 1 else 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 1) (Just (T.Int 0))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
            z = newNthLabel 3
            t = newNthTemp 0
        exp `shouldBe` Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label false) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Label z))))))))
        ty `shouldBe` TUnit

  it "if 0 == 0 then 0 == 0 else 0 == 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Op (T.Int 0) T.Eq (T.Int 0)) (Just (T.Op (T.Int 0) T.Eq (T.Int 1)))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
          id <- getUniqueEff #id
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 1
            false = newNthLabel 2
        exp `shouldBe` Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 0)) (IR.Label false))))
        ty `shouldBe` TUnit

  it "if 0 then x := 0" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let z = newNthLabel 2
            z' = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.Jump (IR.Name z) [z]) (IR.Seq (IR.Label z') (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 0)) (IR.Label z))))
        ty `shouldBe` TUnit

  it "f: () -> (); if 0 == 0 then f()" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.FunApply "f" []) Nothing
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let true = newNthLabel 2
            false = newNthLabel 3
            f = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) true false) (IR.Seq (IR.Label true) (IR.Seq (IR.Exp (IR.Call (IR.Name f) [IR.Temp fp])) (IR.Label false))))
        ty `shouldBe` TUnit

  it "if x(array) then 0" $ do
    let ast = T.expToLExp $ T.If (T.Var (T.Id "x")) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = runEff $ do
          id <- getUniqueEff #id
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

translateRecordCreationSpec :: Spec
translateRecordCreationSpec = describe "translate record creation test" $ do
  it "type record = {}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = newNthNamedLabel "malloc" 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const 0])) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = []}

  it "type record = {x: int}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = newNthNamedLabel "malloc" 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (F.wordSize @FrameMock)])) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 1))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("x", TInt)]}

  it "type record = {x: int, y: string}; record {x = 1, y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1), T.FieldAssign "y" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt), ("y", TString)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = newNthNamedLabel "malloc" 2
            l = newNthLabel 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (2 * (F.wordSize @FrameMock))])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 1)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const (F.wordSize @FrameMock)))) (IR.Name l)))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("x", TInt), ("y", TString)]}

  it "type record = {y: int, x: string}; record {y = 1, x = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "y" (T.Int 1), T.FieldAssign "x" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("y", TInt), ("x", TString)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = newNthNamedLabel "malloc" 2
            l = newNthLabel 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (2 * (F.wordSize @FrameMock))])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 1)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const (F.wordSize @FrameMock)))) (IR.Name l)))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("y", TInt), ("x", TString)]}

  it "type record1 = {}; type record2 = {x: record1};  record2 {x: nil}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record2" [T.FieldAssign "x" T.Nil]
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let record1Ty = TRecord [] id1
              record2Ty = TRecord [("x", record1Ty)] id2
          insertType "record1" record1Ty
          insertType "record2" record2Ty
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            malloc = newNthNamedLabel "malloc" 1
            id1 = newNthUnique 0
            id2 = newNthUnique 1
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (F.wordSize @FrameMock)])) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 0))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id2, map = [("x", TRecord {id = id1, map = []})]}

  it "type record = {x: int}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMissingRecordFieldInConstruction

  it "type record = {}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExtraRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExtraRecordFieldInConstruction

  it "type record = {x: int}; record {y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "y" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMissingRecordFieldInConstruction

  it "type record = {x: int}; record {x = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
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

translateArrayCreationSpec :: Spec
translateArrayCreationSpec = describe "translate array creation test" $ do
  it "type array = array of int; array [0] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) (T.Int 1)
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "array" arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            initArray = newNthNamedLabel "initArray" 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Call (IR.Name initArray) [IR.Const 0, IR.Const 1])) (IR.Temp t))
        ty `shouldBe` TArray {id = id, range = TInt}

  it "type record = {}; type array = array of record; array [0] of nil" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) T.Nil
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let recordTy = TRecord [] id1
              arrayTy = TArray recordTy id2
          insertType "record" recordTy
          insertType "array" arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            initArray = newNthNamedLabel "initArray" 1
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
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "array" arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedType

  it "type array = array of int; array ['hoge'] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.String "hoge") (T.Int 0)
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "array" arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

translateWhileLoopSpec :: Spec
translateWhileLoopSpec = describe "translate while loop test" $ do
  it "while 0 == 0 do x := 0" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let done = newNthLabel 1
            test = newNthLabel 2
            body = newNthLabel 3
        exp `shouldBe` Nx (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done))))))
        ty `shouldBe` TUnit

  it "f: () -> (); while 0 == 0 do f()" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.FunApply "f" [])
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let done = newNthLabel 2
            test = newNthLabel 3
            body = newNthLabel 4
            f = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Exp (IR.Call (IR.Name f) [IR.Temp fp])) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done))))))
        ty `shouldBe` TUnit

  it "while 'hoge' do x := 0" $ do
    let ast = T.expToLExp $ T.While (T.String "hoge") (T.Assign (T.Id "x") (T.Int 0))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

  it "while 0 == 0 do 0" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedUnitType:" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedUnitType

translateForLoopSpec :: Spec
translateForLoopSpec = describe "translate for loop test" $ do
  it "for i := 1 to 2 do x := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) (T.Assign (T.Id "x") (T.Int 3))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            ul = newNthTemp 1
            done = newNthLabel 1
            test = newNthLabel 2
            body = newNthLabel 3
        exp `shouldBe` Nx (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Move (IR.Temp ul) (IR.Const 2)) (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Le (IR.Temp t) (IR.Temp ul) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 3)) (IR.Seq (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done)))))))))
        ty `shouldBe` TUnit

  it "f: () -> (); for i := 1 to 2 do f()" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) (T.FunApply "f" [])
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            ul = newNthTemp 1
            done = newNthLabel 2
            test = newNthLabel 3
            body = newNthLabel 4
            f = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Move (IR.Temp ul) (IR.Const 2)) (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Le (IR.Temp t) (IR.Temp ul) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Exp (IR.Call (IR.Name f) [IR.Temp fp])) (IR.Seq (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done)))))))))
        ty `shouldBe` TUnit

  it "for i := 1 to 2 do 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) (T.Int 3)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedUnitType:" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedUnitType

  it "for i := 'hoge' to 2 do y := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.String "hoge") (T.Int 2) (T.Assign (T.Id "x") (T.Int 3))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType"
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

  it "for i := 1 to 'hoge' do x := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.String "hoge") (T.Assign (T.Id "x") (T.Int 3))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType"
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

translateBreakSpec :: Spec
translateBreakSpec = describe "translate break test" $ do
  it "while 0 == 0 do break" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) T.Break
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let done = newNthLabel 1
            test = newNthLabel 2
            body = newNthLabel 3
        exp `shouldBe` Nx (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Jump (IR.Name done) [done]) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done))))))
        ty `shouldBe` TUnit

  it "while 0 == 0 do (while 0 == 0 do break)" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) T.Break)
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let done1 = newNthLabel 1
            test1 = newNthLabel 5
            body1 = newNthLabel 6
            done2 = newNthLabel 2
            test2 = newNthLabel 3
            body2 = newNthLabel 4
        exp `shouldBe` Nx (IR.Seq (IR.Label test1) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body1 done1) (IR.Seq (IR.Label body1) (IR.Seq (IR.Seq (IR.Label test2) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body2 done2) (IR.Seq (IR.Label body2) (IR.Seq (IR.Jump (IR.Name done2) [done2]) (IR.Seq (IR.Jump (IR.Name test2) [test2]) (IR.Label done2)))))) (IR.Seq (IR.Jump (IR.Name test1) [test1]) (IR.Label done1))))))
        ty `shouldBe` TUnit

  it "for i := 1 to 3 do break" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) T.Break
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            ul = newNthTemp 1
            done = newNthLabel 1
            test = newNthLabel 2
            body = newNthLabel 3
        exp `shouldBe` Nx (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Move (IR.Temp ul) (IR.Const 2)) (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Le (IR.Temp t) (IR.Temp ul) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Jump (IR.Name done) [done]) (IR.Seq (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done)))))))))
        ty `shouldBe` TUnit

  it "break" $ do
    let ast = T.expToLExp T.Break
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return BreakOutsideLoop: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTranslateErrorAndBreakOutsideLoop

  it "(for i := 1 to 3 do x := 2, break)" $ do
    let ast = T.expToLExp $ T.Seq [T.For "i" False (T.Int 1) (T.Int 2) (T.Assign (T.Id "x") (T.Int 3)), T.Break]
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return BreakOutsideLoop: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTranslateErrorAndBreakOutsideLoop

translateFunApplySpec :: Spec
translateFunApplySpec = describe "translate fun application test" $ do
  it "f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TNil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let f = newNthLabel 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp fp])
        ty `shouldBe` TNil

  it "f(1)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Int 0]
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [TInt] TNil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let f = newNthLabel 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp fp, IR.Const 0])
        ty `shouldBe` TNil

  it "type record = {}; f: record -> (); f(nil)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Nil]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord [] id
          insertType "record" recordTy
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [recordTy] TNil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let f = newNthLabel 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp fp, IR.Const 0])
        ty `shouldBe` TNil

  it "f: int -> (); f('hoge')" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.String "hoge"]
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [TInt] TNil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedTypes

  it "f: () -> (); f(0)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Int 0]
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TNil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedTypes

  it "f: int -> (); f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [TInt] TNil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedTypes

  it "var f := (); f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          allocateLocalVariableAndInsertType "f" False TNil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedFunction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedFunction

translateAssignSpec :: Spec
translateAssignSpec = describe "translate assgin test" $ do
  it "var x int; x := 0" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "x") (T.Int 0)
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 1
        exp `shouldBe` Nx (IR.Move (IR.Temp t) (IR.Const 0))
        ty `shouldBe` TUnit

  it "var x string; x := 0" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "x") (T.Int 0)
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TString
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedType

translateSeqSpec :: Spec
translateSeqSpec = describe "translate seq test" $ do
  it "()" $ do
    let ast = T.expToLExp $ T.Seq []
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        ty `shouldBe` TUnit

  it "(1)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1]
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Const 1)
        ty `shouldBe` TInt

  it "(x := 0)" $ do
    let ast = T.expToLExp $ T.Seq [T.Assign (T.Id "x") (T.Int 0)]
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
        exp `shouldBe` Nx (IR.Move (IR.Temp t) (IR.Const 0))
        ty `shouldBe` TUnit

  it "(1, 2)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Int 2]
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.ESeq (IR.Exp (IR.Const 1)) (IR.Const 2))
        ty `shouldBe` TInt

  it "(1, x := 2)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Assign (T.Id "x") (T.Int 2)]
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
        exp `shouldBe` Nx (IR.Seq (IR.Exp (IR.Const 1)) (IR.Move (IR.Temp t) (IR.Const 2)))
        ty `shouldBe` TUnit

  it "(1, ())" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Seq []]
        result = runEff $ translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Nx (IR.Seq (IR.Exp (IR.Const 1)) (IR.Exp (IR.Const 0)))
        ty `shouldBe` TUnit

translateLetSpec :: Spec
translateLetSpec = describe "translate let test" $ do
  it "let in 0" $ do
    let ast = T.expToLExp $ T.Let [] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let in ()" $ do
    let ast = T.expToLExp $ T.Let [] (T.Seq [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Nx (IR.Exp (IR.Const 0))
        ty `shouldBe` TUnit
        fragments.fragments `shouldBe` []

  it "let in 1 == 2" $ do
    let ast = T.expToLExp $ T.Let [] (T.Op (T.Int 1) T.Eq (T.Int 2))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Cx (\true false -> IR.CJump IR.Eq (IR.Const 1) (IR.Const 2) true false)
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let var x: int := 0 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 0)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Temp t))
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let var x := 0 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Int 0)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Temp t))
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let var x := () in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Seq [])] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Temp t))
        ty `shouldBe` TUnit
        fragments.fragments `shouldBe` []

  it "let var x := 0 in 1 == 2" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Int 0)] (T.Op (T.Int 1) T.Eq (T.Int 2))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
        exp `shouldBe` Cx (\true false -> IR.Seq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.CJump IR.Eq (IR.Const 1) (IR.Const 2) true false))
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let var x: int := 0 in x := 1" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 0)] (T.Assign (T.Id "x") (T.Int 1))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
        exp `shouldBe` Nx (IR.Seq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Move (IR.Temp t) (IR.Const 1)))
        ty `shouldBe` TUnit
        fragments.fragments `shouldBe` []

  it "let var x: int := let var y: int := 0 in y in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Let [T.VarDec "y" False (Just "int") (T.Int 0)] (T.Var (T.Id "y")))] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t0 = newNthTemp 0
            t1 = newNthTemp 1
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t1) (IR.ESeq (IR.Move (IR.Temp t0) (IR.Const 0)) (IR.Temp t0))) (IR.Temp t1))
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let var x: int = 1; var x: int = 2 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 1), T.VarDec "x" False (Just "int") (T.Int 2)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t0 = newNthTemp 0
            t1 = newNthTemp 1
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t0) (IR.Const 1)) (IR.Move (IR.Temp t1) (IR.Const 2))) (IR.Temp t1))
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let var: int x = 1; var y: int = let var x: int := 2 in x in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 1), T.VarDec "y" False (Just "int") (T.Let [T.VarDec "x" False (Just "int") (T.Int 2)] (T.Var (T.Id "x")))] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t0 = newNthTemp 0
            t1 = newNthTemp 1
            t2 = newNthTemp 2
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t0) (IR.Const 1)) (IR.Move (IR.Temp t2) (IR.ESeq (IR.Move (IR.Temp t1) (IR.Const 2)) (IR.Temp t1)))) (IR.Temp t0))
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let type record = {} in record {}" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "record" (T.RecordType [])] (T.RecordCreate "record" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
            malloc = newNthNamedLabel "malloc" 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const 0])) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = []}
        fragments.fragments `shouldBe` []

  it "let type record = {y: int, x: string} in record {y: 0, x: 'hoge'}" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "record" (T.RecordType [T.Field "y" False "int", T.Field "x" False "string"])] (T.RecordCreate "record" [T.FieldAssign "y" (T.Int 0), T.FieldAssign "x" (T.String "hoge")])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
            malloc = newNthNamedLabel "malloc" 2
            l = newNthLabel 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (2 * (F.wordSize @FrameMock))])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 0)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const (F.wordSize @FrameMock)))) (IR.Name l)))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("y", TInt), ("x", TString)]}
        length fragments.fragments `shouldBe` 1
        (fragments.fragments Partial.!! 0).string.text `shouldBe` Just "hoge"

  it "let function f(x: int): int = x in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Var (T.Id "x"))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp fp, IR.Const 1])
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Temp t))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0, InReg t]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let function f(x: int): int = let var y: int = 1 in x + y in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Let [T.VarDec "y" True (Just "int") (T.Int 1)] (T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "y"))))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
            f = newNthNamedLabel "f" 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp fp, IR.Const 1])
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.ESeq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 1)) (IR.BinOp IR.Plus (IR.Temp t) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))))))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0, InReg t]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 1
        (fragments.fragments Partial.!! 0).procedure.frame.localVariables `shouldBe` Just [InFrame (-4)]

  it "let function f(x: int): int = let function g(y: int): int = x + y in g(0) in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" True "int"] (Just "int") (T.Let [T.FunDec "g" [T.Field "y" False "int"] (Just "int") (T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "y")))] (T.FunApply "g" [T.Int 0]))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            g = newNthNamedLabel "g" 2
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp fp, IR.Const 1])
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 2
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp))))) (IR.Temp t)))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0, InReg t]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0
        (fragments.fragments Partial.!! 1).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Call (IR.Name g) [IR.Temp fp, IR.Const 0]))
        (fragments.fragments Partial.!! 1).procedure.frame.formals `shouldBe` Just [InFrame 0, InFrame (-4)]
        (fragments.fragments Partial.!! 1).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let type a = record{}; function f(): a = nil in f()" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType []), T.FunDec "f" [] (Just "a") T.Nil] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp fp])
        ty `shouldBe` TRecord {id = id, map = []}
        length fragments.fragments `shouldBe` 1
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Const 0))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let type myint = int; var a: myint = 1; function f(): myint = a; in f()" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "myint" (T.TypeId "int"), T.VarDec "x" True (Just "myint") (T.Int 1), T.FunDec "f" [] (Just "myint") (T.Var (T.Id "x"))] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 1)) (IR.Call (IR.Name f) [IR.Temp fp]))
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp))))))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let var f: int = 1; function f(): int = 1 in f()" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "f" False (Just "int") (T.Int 1), T.FunDec "f" [] (Just "int") (T.Int 1)] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Call (IR.Name f) [IR.Temp fp]))
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Const 1))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let function f(): int = 1; var f: int = 1 in f" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] (Just "int") (T.Int 1), T.VarDec "f" False (Just "int") (T.Int 1)] (T.Var (T.Id "f"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Temp t))
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Const 1))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let function f() = g(); function g() = f() in f()" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] Nothing (T.FunApply "g" []), T.FunDec "g" [] Nothing (T.FunApply "f" [])] (T.FunApply "g" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            g = newNthNamedLabel "g" 2
        exp `shouldBe` Ex (IR.Call (IR.Name g) [IR.Temp fp])
        ty `shouldBe` TUnit
        length fragments.fragments `shouldBe` 2
        (fragments.fragments Partial.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Call (IR.Name g) [IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp))]))
        (fragments.fragments Partial.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments Partial.!! 0).procedure.frame.numberOfLocals
          `shouldBe` Just 0
        (fragments.fragments Partial.!! 1).procedure.body `shouldBe` Just (IR.Move (IR.Temp rv) (IR.Call (IR.Name f) [IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp))]))
        (fragments.fragments Partial.!! 1).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments Partial.!! 1).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let type a = {a: b}; type b = {b: a}; var x: a = nil in x" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType [T.Field "a" False "b"]), T.TypeDec "b" (T.RecordType [T.Field "b" False "a"]), T.VarDec "x" False (Just "a") T.Nil] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 0)) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("a", TName (dummyRealLocated "b"))]}

  it "let type intlist = {hd: int, tl: intlist}; var list:intlist := intlist {hd: 0, tl: nil} in list" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "intlist" (T.RecordType [T.Field "hd" False "int", T.Field "tl" False "intlist"]), T.VarDec "list" False (Just "intlist") (T.RecordCreate "intlist" [T.FieldAssign "hd" (T.Int 0), T.FieldAssign "tl" T.Nil])] (T.Var (T.Id "list"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t0 = newNthTemp 0
            t1 = newNthTemp 1
            malloc = newNthNamedLabel "malloc" 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t1) (IR.ESeq (IR.Seq (IR.Move (IR.Temp t0) (IR.Call (IR.Name malloc) [IR.Const (2 * (F.wordSize @FrameMock))])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t0) (IR.Const 0))) (IR.Const 0)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t0) (IR.Const (F.wordSize @FrameMock)))) (IR.Const 0)))) (IR.Temp t0))) (IR.Temp t1))
        ty `shouldBe` TRecord {id = id, map = [("hd", TInt), ("tl", TName (dummyRealLocated "intlist"))]}

  it "let var x: myint := 0 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "myint") (T.Int 0)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UnknownType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUnknownType

  it "let x := let y := 0 in y in y" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Let [T.VarDec "y" False Nothing (T.Int 0)] (T.Var (T.Id "y")))] (T.Var (T.Id "y"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UndefinedVariable: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUndefinedVariable

  it "let type a = b; type b = c; type c = a in 0" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.TypeId "b"), T.TypeDec "b" (T.TypeId "c"), T.TypeDec "c" (T.TypeId "a")] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return InvalidRecTypeDeclaration: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndInvalidRecTypeDeclaration

  it "type a = {a: b}; var x = 0; type b = {b: a} in x" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType [T.Field "a" False "b"]), T.VarDec "x" False Nothing (T.Int 0), T.TypeDec "b" (T.RecordType [T.Field "b" False "a"])] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UnknownType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUnknownType

  it "let function f() = g(); var x := 0; function g() = f() in f()" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] Nothing (T.FunApply "g" []), T.VarDec "x" False Nothing (T.Int 0), T.FunDec "g" [] Nothing (T.FunApply "f" [])] (T.FunApply "g" [])
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UndefinedVariale: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUndefinedVariable

  it "let var x: int = 'hoge' in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.String "hoge")] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedType

  it "let function f(x: hoge): int = 0 in 0" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "hoge"] (Just "int") (T.Int 0)] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UnknownType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUnknownType

  it "let function f(): int = 0; function f(): int = 0 in 0" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] (Just "int") (T.Int 0), T.FunDec "f" [] (Just "int") (T.Int 0)] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MultiDeclaredName: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName

  it "let type a = int; type a = string in 0" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.TypeId "int"), T.TypeDec "a" (T.TypeId "int")] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MultiDeclaredName: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName

  it "let function f(x: int): int = x; function g(): int = x in g()" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Var (T.Id "x")), T.FunDec "g" [] (Just "int") (T.Var (T.Id "x"))] (T.FunApply "g" [])
        result = runEff $ translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UndefinedVariable: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUndefinedVariable

  it "let x = let function f(x: int): int = x in f(1) in f(2)" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Var (T.Id "x"))] (T.FunApply "f" [T.Int 1]))] (T.FunApply "f" [T.Int 2])
        result = runEff $ translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UndefinedVariable: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndUndefinedVariable

runEff ::
  Eff
    '[ ("typeEnv" >: State TEnv),
       ("varTypeEnv" >: State VTEnv),
       ("varAccessEnv" >: State (VAEnv FrameMock)),
       ("nestingLevel" >: NestingLevelEff FrameMock),
       ("breakpoint" >: BreakPointEff),
       ("fragment" >: F.ProgramEff FrameMock),
       ("id" >: UniqueEff),
       ("typeCheckError" >: EitherEff (RealLocated TypeCheckError)),
       ("translateError" >: EitherEff (RealLocated TranslateError)),
       ("temp" >: UniqueEff),
       ("label" >: UniqueEff)
     ]
    a ->
  Either (RealLocated SemantAnalysisError) ((a, NestingLevel FrameMock), F.ProgramFragments FrameMock)
runEff a = fst . fst . leaveEff . runUniqueEff @"label" uniqueSeed . runUniqueEff @"temp" uniqueSeed . runTranslateEff $ do
  label <- newLabel
  withNewLevelEff label [] a

allocateLocalVariableAndInsertType ::
  ( F.Frame f,
    Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "varAccessEnv" (State (VAEnv f)),
    Lookup xs "nestingLevel" (NestingLevelEff f),
    Lookup xs "temp" UniqueEff
  ) =>
  Id ->
  Bool ->
  Type ->
  Eff xs ()
allocateLocalVariableAndInsertType name escape ty = do
  allocateLocalVariable name escape
  insertVarType name $ VarType ty

insertFun ::
  ( F.Frame f,
    Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "varAccessEnv" (State (VAEnv f))
  ) =>
  Id ->
  Label ->
  Level f ->
  [Type] ->
  Type ->
  Eff xs ()
insertFun name label parent domains codomain = do
  insertVarAccess name $ FunAccess label parent
  insertVarType name $ FunType domains codomain
