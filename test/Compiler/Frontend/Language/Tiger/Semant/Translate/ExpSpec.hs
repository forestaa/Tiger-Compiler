module Compiler.Frontend.Language.Tiger.Semant.Translate.ExpSpec (spec) where

import Compiler.Frontend.FrameMock (FrameMock)
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant (translateExp)
import Compiler.Frontend.Language.Tiger.Semant.Env (insertType)
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level (fetchCurrentLevelEff)
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (allocateLocalVariableAndInsertType, fetchTwoLabel, insertFun, isTypeCheckErrorAndExpectedExpression, isTypeCheckErrorAndExpectedFunction, isTypeCheckErrorAndExpectedIntType, isTypeCheckErrorAndExpectedType, isTypeCheckErrorAndExpectedTypes, isTypeCheckErrorAndNotDeterminedNilType, runEff)
import Compiler.Frontend.SrcLoc (RealLocated (L), dummyRealLocated)
import Compiler.Intermediate.Frame (ProgramFragments (fragments), StringFragment (text))
import Compiler.Intermediate.Frame qualified as F (fp, wordSize)
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U (externalLabel, getUniqueEff, newLabel)
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthTemp)
import Compiler.Utils.Maybe ()
import RIO
import RIO.List.Partial qualified as List (head)
import RIO.Text qualified as Text (unpack)
import Test.Hspec

spec :: Spec
spec = do
  translateIntSpec
  translateStringSpec
  translateBinOpSpec
  translateFunApplySpec

translateIntSpec :: Spec
translateIntSpec = describe "translate int test" $ do
  it "translate 0" $ do
    let ast = T.expToLExp $ T.Int 0
    case runEff (translateExp ast) of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt

translateStringSpec :: Spec
translateStringSpec = describe "translate string test" $ do
  it "translate 'hoge'" $ do
    let ast = T.expToLExp $ T.String "hoge"
    case runEff (translateExp ast) of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Ex (IR.Name (newNthLabel 1))
        ty `shouldBe` TString
        length fragments.fragments `shouldBe` 1
        (List.head fragments.fragments).string.text `shouldBe` Just "hoge"

translateBinOpSpec :: Spec
translateBinOpSpec = describe "translate binop test" $ do
  it "0 + 0" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedIntType

  it "x == x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "x"))
        result = runEff $ do
          id <- U.getUniqueEff #id
          let arrayTy = TArray TInt id
          allocateLocalVariableAndInsertType "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
          id <- U.getUniqueEff #id
          let recordTy = TRecord [("x", TInt)] id
          allocateLocalVariableAndInsertType "x" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((Cx genstm, ty), _), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Ne (IR.Call (IR.Name (U.externalLabel "stringEqual")) [IR.Name (newNthLabel 1), IR.Name (newNthLabel 2)]) (IR.Const 0) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "'hoge' <> 'hoge'" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.NEq (T.String "hoge")
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((Cx genstm, ty), _), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Call (IR.Name (U.externalLabel "stringEqual")) [IR.Name (newNthLabel 1), IR.Name (newNthLabel 2)]) (IR.Const 0) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "x == y (array == record)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "y"))
        result = runEff $ do
          id1 <- U.getUniqueEff #id
          id2 <- U.getUniqueEff #id
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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

translateFunApplySpec :: Spec
translateFunApplySpec = describe "translate fun application test" $ do
  it "f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TNil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let f = newNthLabel 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock)])
        ty `shouldBe` TNil

  it "f(1)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Int 0]
        result = runEff $ do
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [TInt] TNil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let f = newNthLabel 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock), IR.Const 0])
        ty `shouldBe` TNil

  it "type record = {}; f: record -> (); f(nil)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Nil]
        result = runEff $ do
          id <- U.getUniqueEff #id
          let recordTy = TRecord [] id
          insertType "record" recordTy
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [recordTy] TNil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let f = newNthLabel 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock), IR.Const 0])
        ty `shouldBe` TNil
  it "external(1)" $ do
    let ast = T.expToLExp $ T.FunApply "external" [T.Int 0]
        result = runEff $ translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let external = U.externalLabel "external"
        exp `shouldBe` Ex (IR.Call (IR.Name external) [IR.Const 0])
        ty `shouldBe` TInt

  it "f: int -> (); f('hoge')" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.String "hoge"]
        result = runEff $ do
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [TInt] TNil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedTypes

  it "f: () -> (); f(0)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Int 0]
        result = runEff $ do
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TNil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isTypeCheckErrorAndExpectedTypes

  it "f: int -> (); f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          label <- U.newLabel
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
