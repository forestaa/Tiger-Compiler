module Tiger.SemantSpec (spec) where

import RIO
import Test.Hspec

import Tiger.Semant
import Tiger.Semant.Translate
import Tiger.Semant.Types
import qualified Tiger.LSyntax as T (expToLExp)
import qualified Tiger.Syntax as T
import qualified Frame as F
import FrameMock
import qualified IR
import SrcLoc
import Unique

import Data.Extensible
import qualified RIO.List.Partial as Partial
import qualified RIO.Map as Map


spec :: Spec
spec = do
  translateIntSpec
  translateStringSpec
  translateNilSpec
  translateVariableSpec
  translateRecordFieldSpec
  translateArrayIndexSpec

translateIntSpec :: Spec
translateIntSpec = describe "translate int test" $ do
  it "translate 0" $ do
    let ast = T.expToLExp $ T.Int 0
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt

translateStringSpec :: Spec
translateStringSpec = describe "translate string test" $ do
  it "translate 'hoge'" $ do
    let ast = T.expToLExp $ T.String "hoge"
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TString
        length fragments `shouldBe` 1
        Partial.head fragments `shouldSatisfy` fragmentP
    where
      expP (Ex (IR.Name _)) = True
      expP _ = False
      fragmentP (F.String (Label _ _) s) = s == "hoge"
      fragmentP _ = False

translateNilSpec :: Spec
translateNilSpec = describe "translate nil test" $ do
  it "translate nil" $ do
    let ast = T.expToLExp $ T.Nil
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left e -> expectationFailure $ show e
      Right ((_, ty), _) -> do
        ty `shouldBe` TNil

translateVariableSpec :: Spec
translateVariableSpec = describe "translate variable test" $ do
  it "first local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          _ <- allocateLocalVariable "y" True TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-2*F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable, first is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" False TInt
          _ <- allocateLocalVariable "y" True TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "first local variable, second is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          _ <- allocateLocalVariable "y" False TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "local variable, not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` inRegister
        ty `shouldBe` TInt
        where
          inRegister (Ex (IR.Temp _)) = True
          inRegister _ = False

  it "undefined variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUndefinedVariable
        where
          isUndefinedVariable (VariableUndefined id) = id == "x"
          isUndefinedVariable _ = False

  it "variable referes function" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          insertVar "x" $ Fun undefined
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedVariable
        where
          isExpectedVariable (ExpectedVariable id) = id == "x"
          isExpectedVariable _ = False


translateRecordFieldSpec :: Spec
translateRecordFieldSpec = describe "translate record field test" $ do
  it "first record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id <: nil
          _ <- allocateLocalVariable "object" True recordTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)))
        ty `shouldBe` TInt

  it "second record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "y")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt), ("y", TString)] <: #id @= id <: nil
          _ <- allocateLocalVariable "object" True recordTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const (F.wordSize @FrameMock))))
        ty `shouldBe` TString

  it "type synonym for record type" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id <: nil
              nameTy = TName (dummyRealLocated "record")
          insertType "record" recordTy
          _ <- allocateLocalVariable "object" True nameTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)))
        ty `shouldBe` TInt

  it "not record type" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "object" True TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedRecordType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedRecordType
        where
          isExpectedRecordType ExpectedRecordType{} = True
          isExpectedRecordType _ = False

  it "missing record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "z")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt), ("y", TString)] <: #id @= id <: nil
          _ <- allocateLocalVariable "object" True recordTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordField: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMissingRecordField
        where
          isMissingRecordField MissingRecordField{} = True
          isMissingRecordField _ = False


translateArrayIndexSpec :: Spec
translateArrayIndexSpec = describe "translate array index test" $ do
  it "array index x[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x[1 + 1]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Op (T.Int 1) T.Plus (T.Int 1)))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.BinOp IR.Plus (IR.Const 1) (IR.Const 1)) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x.y[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.RecField (T.Id "x") "y") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TString <: #id @= id1 <: nil
              recordTy = TRecord $ #map @= Map.fromList [("y", arrayTy)] <: #id @= id2 <: nil
          _ <- allocateLocalVariable "x" True recordTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TString

  it "type synonym for array type" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
              nameTy = TName (dummyRealLocated "array")
          insertType "array" arrayTy
          _ <- allocateLocalVariable "x" True nameTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x['hoge']" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.String "hoge"))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType" ++ show ret
      Left e -> e `shouldSatisfy` isExpectedIntType
        where
          isExpectedIntType (L _ ExpectedIntType{}) = True
          isExpectedIntType _ = False

  it "array type expected" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedArrayType" ++ show ret
      Left e -> e `shouldSatisfy` isExpectedIntType
        where
          isExpectedIntType (L _ ExpectedArrayType{}) = True
          isExpectedIntType _ = False
