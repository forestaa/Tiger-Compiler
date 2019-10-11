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
  translateIfNoElseSpec
  translateRecordCreationSpec
  translateArrayCreationSpec
  translateWhileLoopSpec
  translateForLoopSpec

translateIntSpec :: Spec
translateIntSpec = describe "translate int test" $ do
  it "translate 0" $ do
    let ast = T.expToLExp $ T.Int 0
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt

translateStringSpec :: Spec
translateStringSpec = describe "translate string test" $ do
  it "translate 'hoge'" $ do
    let ast = T.expToLExp $ T.String "hoge"
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "local variable, not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
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

  it "variable referes function" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          insertVar "x" $ Fun undefined
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedVariable


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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "array type expected" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedArrayType" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedArrayType

translateBinOpSpec :: Spec
translateBinOpSpec = describe "translate binop test" $ do
  it "0 + 0" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Int 0)
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.BinOp IR.Plus (IR.Const 0) (IR.Const 0))
        ty `shouldBe` TInt

  it "'hoge' + 1" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Plus (T.Int 0)
        result = leaveEff. runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "x + x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "x"))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "x == x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "x"))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> e `shouldSatisfy` isExpectedType

  it "0 + (0 == 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Op (T.Int 0) T.Eq (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> e `shouldSatisfy` isExpectedExpression

translateIfElseSpec :: Spec
translateIfElseSpec = describe "translate if-else test" $ do
  it "if 0 == 0 then 1 else 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 1) (Just (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> expectationFailure $ show e
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
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

translateIfNoElseSpec :: Spec
translateIfNoElseSpec = describe "translate if-no-else test" $ do
  it "if 0 == 0 then x := 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) t z) (IR.Seq (IR.Label t') (IR.Seq (IR.Move _ (IR.Const 0)) (IR.Label z'))))) = t == t' && z == z'
          expP _ = False

  it "if 0 then x := 0" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.Jump (IR.Name z) _) (IR.Seq (IR.Label _) (IR.Seq (IR.Move _ (IR.Const 0)) (IR.Label z'))))) = z == z'
          expP _ = False

  it "if x(array) then 0" $ do
    let ast = T.expToLExp $ T.If (T.Var (T.Id "x")) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "if 0 then 0" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Int 0) Nothing
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedUnitType


translateRecordCreationSpec :: Spec
translateRecordCreationSpec = describe "translate record creation test" $ do
  it "type record = {}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const 0])) (IR.Temp _))) = True
          expP _ = False
          tyP (TRecord r) = r ^. #map == Map.fromList []
          tyP _ = False

  it "type record = {x: int}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const n])) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const 0))) (IR.Const 1))) (IR.Temp _))) = n == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = r ^. #map == Map.fromList [("x", TInt)]
          tyP _ = False

  it "type record = {x: int, y: string}; record {x = 1, y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1), T.FieldAssign "y" (T.String "hoge")]
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt), ("y", TString)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const n])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const 0))) (IR.Const 1)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const i))) (IR.Name _)))) (IR.Temp _))) = n == 2 * F.wordSize @FrameMock && i == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = r ^. #map == Map.fromList [("x", TInt), ("y", TString)]
          tyP _ = False

  it "type record = {x: int}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMissingRecordFieldInConstruction

  it "type record = {}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExtraRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExtraRecordFieldInConstruction

  it "type record = {x: int}; record {y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "y" (T.String "hoge")]
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMissingRecordFieldInConstruction

  it "type record = {x: int}; record {x = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.String "hoge")]
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= Map.fromList [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeForRecordField: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedTypeForRecordField

  it "type myint = int; myint {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "myint" []
        result = leaveEff . runTranslateEffWithNewLevel $ do
          insertType "myint" TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedRecordType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedRecordType


translateArrayCreationSpec :: Spec
translateArrayCreationSpec = describe "translate array creation test" $ do
  it "type array = array of int; array [0] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) (T.Int 1)
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          insertType "array" arrayTy
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy`isIntArray
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const 0, IR.Const 1])) (IR.Temp _))) = True
          expP _ = False
          isIntArray (TArray r) = r ^. #range == TInt
          isIntArray _ = False

  it "type myint = int; myint [0] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "myint" (T.Int 0) (T.Int 1)
        result = leaveEff . runTranslateEffWithNewLevel $ do
          insertType "myint" TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedArrayType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedArrayType

  it "type array = array of int; array [0] of 'hoge'" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) (T.String "hoge")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          insertType "array" arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedType

  it "type array = array of int; array ['hoge'] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.String "hoge") (T.Int 0)
        result = leaveEff . runTranslateEffWithNewLevel $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          insertType "array" arrayTy
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType


translateWhileLoopSpec :: Spec
translateWhileLoopSpec = describe "translate while loop test" $ do
  it "while 0 == 0 do x := 0" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body done) (IR.Seq (IR.Label body') (IR.Seq (IR.Move (IR.Mem _) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name test') _) (IR.Label done'))))))) = test == test' && body == body' && done == done'
          expP _ = False

  it "while 'hoge' do x := 0" $ do
    let ast = T.expToLExp $ T.While (T.String "hoge") (T.Assign (T.Id "x") (T.Int 0))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "while 0 == 0 do 0" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 0)
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedUnitType:" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedUnitType


translateForLoopSpec :: Spec
translateForLoopSpec = describe "translate for loop test" $ do
  it "for i := 1 to 2 do x := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) (T.Assign (T.Id "x") (T.Int 3))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.Move (IR.Temp r) (IR.Const 1)) (IR.Seq (IR.Move (IR.Temp ul) (IR.Const 2)) (IR.Seq (IR.Label loop) (IR.Seq (IR.CJump IR.Le (IR.Temp r') (IR.Temp ul') body done) (IR.Seq (IR.Label body') (IR.Seq (IR.Move (IR.Mem _) (IR.Const 3)) (IR.Seq (IR.Move (IR.Temp r'') (IR.BinOp IR.Plus (IR.Temp r''') (IR.Const 1))) (IR.Seq (IR.Jump (IR.Name loop') _) (IR.Label done')))))))))) = r == r' && r == r'' && r == r''' && ul == ul' && body == body' && done == done' && loop == loop'
          expP _ = False

  it "for i := 1 to 2 do 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) (T.Int 3)
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedUnitType:" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedUnitType

  it "for i := 'hoge' to 2 do y := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.String "hoge") (T.Int 2) (T.Assign (T.Id "x") (T.Int 3))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType"
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "for i := 1 to 'hoge' do x := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.String "hoge") (T.Assign (T.Id "x") (T.Int 3))
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType"
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType


isExpectedVariable :: TranslateError -> Bool
isExpectedVariable ExpectedVariable{} = True
isExpectedVariable _ = False
isUndefinedVariable :: TranslateError -> Bool
isUndefinedVariable VariableUndefined{} = True
isUndefinedVariable _ = False
isExpectedExpression :: TranslateError -> Bool
isExpectedExpression ExpectedExpression{} = True
isExpectedExpression _ = False
isExpectedIntType :: TranslateError -> Bool
isExpectedIntType ExpectedIntType{} = True
isExpectedIntType _ = False
isExpectedUnitType :: TranslateError -> Bool
isExpectedUnitType ExpectedUnitType{} = True
isExpectedUnitType _ = False
isExpectedArrayType :: TranslateError -> Bool
isExpectedArrayType ExpectedArrayType{} = True
isExpectedArrayType _ = False
isExpectedRecordType :: TranslateError -> Bool
isExpectedRecordType ExpectedRecordType{} = True
isExpectedRecordType _ = False
isExpectedType :: TranslateError -> Bool
isExpectedType ExpectedType{} = True
isExpectedType _ = False
isExpectedTypeForRecordField :: TranslateError -> Bool
isExpectedTypeForRecordField ExpectedTypeForRecordField{} = True
isExpectedTypeForRecordField _ = False
isExtraRecordFieldInConstruction :: TranslateError -> Bool
isExtraRecordFieldInConstruction ExtraRecordFieldInConstruction{} = True
isExtraRecordFieldInConstruction _ = False
isMissingRecordFieldInConstruction :: TranslateError -> Bool
isMissingRecordFieldInConstruction MissingRecordFieldInConstruction{} = True
isMissingRecordFieldInConstruction _ = False
isMissingRecordField :: TranslateError -> Bool
isMissingRecordField MissingRecordField{} = True
isMissingRecordField _ = False
