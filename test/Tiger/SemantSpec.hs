module Tiger.SemantSpec (spec) where

import RIO
import Test.Hspec

import Tiger.Semant
import Tiger.Semant.Translate
import Tiger.Semant.Types
import Tiger.TestUtils
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
  translateBinOpSpec
  translateIfElseSpec

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
      Left e -> e `shouldSatisfy` isExpectedArrayType
        where
          isExpectedArrayType (L _ ExpectedArrayType{}) = True
          isExpectedArrayType _ = False

translateBinOpSpec :: Spec
translateBinOpSpec = describe "translate binop test" $ do
  it "0 + 0" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Int 0)
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.BinOp IR.Plus (IR.Const 0) (IR.Const 0))
        ty `shouldBe` TInt

  it "'hoge' + 1" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Plus (T.Int 0)
        result = leaveEff. runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left e -> e `shouldSatisfy` isExpectedIntType
        where
          isExpectedIntType (L _ ExpectedIntType{}) = True
          isExpectedIntType _ = False

  it "x + x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "x"))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left e -> e `shouldSatisfy` isExpectedIntType
        where
          isExpectedIntType (L _ ExpectedIntType{}) = True
          isExpectedIntType _ = False

  it "x == x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "x"))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "nil ≠ nil" $ do
    let ast = T.expToLExp $ T.Op T.Nil T.NEq T.Nil
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Ne (IR.Const 0) (IR.Const 0) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "nil == x (record)" $ do
    let ast = T.expToLExp $ T.Op T.Nil T.Eq (T.Var (T.Id "x"))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True recordTy
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Const 0) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "x == y (array == record)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "y"))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id1 <: nil
              arrayTy = TArray  $ #range @= TInt <: #id @= id2 <: nil
          _ <- allocateLocalVariable "x" True recordTy
          _ <- allocateLocalVariable "y" True arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left e -> e `shouldSatisfy` isExpectedType
        where
          isExpectedType (L _ ExpectedType{}) = True
          isExpectedType _ = False

  it "0 + (0 == 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Op (T.Int 0) T.Eq (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.BinOp IR.Plus (IR.Const 0) (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Const 1)) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) t f) (IR.Seq (IR.Label f') (IR.Seq (IR.Move (IR.Temp _) (IR.Const 0)) (IR.Label t'))))) (IR.Temp _)))) = t == t' && f == f'
          expP _ = False

  it "0 == (0 == 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Eq (T.Op (T.Int 0) T.Eq (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (IR.CJump IR.Eq (IR.Const 0) (IR.ESeq (IR.Seq (IR.Move _ (IR.Const 1)) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) t f) (IR.Seq (IR.Label f') (IR.Seq (IR.Move _ (IR.Const 0)) (IR.Label t'))))) (IR.Temp _)) _ _) = t == t' && f == f'
          expP _ = False
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "(x := 0) == (x := 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Assign (T.Id "x") (T.Int 0)) T.Eq (T.Assign (T.Id "x") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedExpression: " ++ show ret
      Left e -> e `shouldSatisfy` isExpectedExpression
        where
          isExpectedExpression (L _ ExpectedExpression{}) = True
          isExpectedExpression _ = False

translateIfElseSpec :: Spec
translateIfElseSpec = describe "translate if-else test" $ do
  it "if 0 == 0 then 1 else 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 1) (Just (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) t f) (IR.Seq (IR.Label t') (IR.Seq (IR.Move (IR.Temp _) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z) _) (IR.Seq (IR.Label f') (IR.Seq (IR.Move (IR.Temp _) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z') _) (IR.Label z'')))))))) (IR.Temp _))) = t == t' && f == f' && z == z'' && z' == z''
          expP _ = False

  it "if 0 == 0 then x := 0 else x := 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0)) (Just (T.Assign (T.Id "x") (T.Int 1)))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) t f) (IR.Seq (IR.Label t') (IR.Seq (IR.Move _ (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) _) (IR.Seq (IR.Label f')(IR.Seq (IR.Move _ (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z') _) (IR.Label z''))))))))) = t == t' && f == f' && z == z'' && z' == z''
          expP _ = False

  it "if 0 == 0 then 0 == 0 else 0 == 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Op (T.Int 0) T.Eq (T.Int 0)) (Just (T.Op (T.Int 0) T.Eq (T.Int 1)))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) t f) (IR.Seq (IR.Label t') (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) ret0 ret1) (IR.Seq (IR.Label f') (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 1) ret0' ret1') (IR.Seq (IR.Label ret0'') (IR.Seq (IR.Move (IR.Temp _) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) _) (IR.Seq (IR.Label ret1'') (IR.Seq (IR.Move (IR.Temp _) (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z') _) (IR.Label z'')))))))))))) (IR.Temp _))) = t == t' && f == f' && ret0 == ret0' && ret0 == ret0'' && ret1 == ret1' && ret1 == ret1'' && z == z' && z == z''
          expP _ = False

  it "if 0 == 0 then 0 else 0 == 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 0) (Just (T.Op (T.Int 0) T.Eq (T.Int 1)))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) t f) (IR.Seq (IR.Label t') (IR.Seq (IR.Move (IR.Temp _) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) _) (IR.Seq (IR.Label f') (IR.Seq (IR.Move (IR.Temp _) (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Const 1)) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 1) t'' f'') (IR.Seq (IR.Label f''') (IR.Seq (IR.Move (IR.Temp _) (IR.Const 0)) (IR.Label t'''))))) (IR.Temp _))) (IR.Seq (IR.Jump (IR.Name z') _) (IR.Label z'')))))))) (IR.Temp _))) = t == t' && f == f' && t'' == t''' && f'' == f''' && z == z' && z == z''
          expP _ = False


  it "if 0 then 0 else 1" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Int 0) (Just (T.Int 1))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Jump (IR.Name f) _) (IR.Seq (IR.Label _) (IR.Seq (IR.Move _ (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) _) (IR.Seq (IR.Label f') (IR.Seq (IR.Move _ (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z') _) (IR.Label z'')))))))) (IR.Temp _))) = f == f' && z == z' && z == z''
          expP _ = False

  it "if 1 + 1 then 0 else 1" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 1) T.Plus (T.Int 1)) (T.Int 0) (Just (T.Int 1))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.CJump IR.Ne (IR.BinOp IR.Plus (IR.Const 1) (IR.Const 1)) (IR.Const 0) t f) (IR.Seq (IR.Label t') (IR.Seq (IR.Move _ (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name z) _) (IR.Seq (IR.Label f') (IR.Seq (IR.Move _ (IR.Const 1)) (IR.Seq (IR.Jump (IR.Name z') _) (IR.Label z'')))))))) (IR.Temp _))) = t == t' && f == f' && z == z' && z == z''
          expP _ = False

  it "if x(array) then 0 else 1" $ do
    let ast = T.expToLExp $ T.If (T.Var (T.Id "x")) (T.Assign (T.Id "x") (T.Int 0)) (Just (T.Assign (T.Id "x") (T.Int 1)))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left e -> e `shouldSatisfy` isExpectedIntType
        where
          isExpectedIntType (L _ ExpectedIntType{}) = True
          isExpectedIntType _ = False
