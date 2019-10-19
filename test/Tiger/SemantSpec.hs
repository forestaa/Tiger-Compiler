module Tiger.SemantSpec (spec) where

import           Data.Extensible
import           RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as Partial
import qualified RIO.Map as Map
import           Test.Hspec

import qualified Frame as F
import           FrameMock
import qualified IR
import           SrcLoc
import           TestUtils
import           Unique

import           Tiger.Semant
import           Tiger.Semant.BreakPoint
import           Tiger.Semant.Env
import           Tiger.Semant.Exp
import           Tiger.Semant.Level
import           Tiger.Semant.Translate
import           Tiger.Semant.Types
import qualified Tiger.LSyntax as T (expToLExp)
import qualified Tiger.Syntax as T



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
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt

translateStringSpec :: Spec
translateStringSpec = describe "translate string test" $ do
  it "translate 'hoge'" $ do
    let ast = T.expToLExp $ T.String "hoge"
    case runEff (translateExp ast) of
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
    case runEff (translateExp ast) of
      Left (L _ e) -> expectationFailure $ show e
      Right ((_, ty), _) -> do
        ty `shouldBe` TNil

translateVariableSpec :: Spec
translateVariableSpec = describe "translate variable test" $ do
  it "first local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          _ <- allocateLocalVariable "y" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-2*F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable, first is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          _ <- allocateLocalVariable "y" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "first local variable, second is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          _ <- allocateLocalVariable "y" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "local variable, not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp ast
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
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUndefinedVariable

  it "variable referes function" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = runEff $ do
          insertVar "x" $ Fun undefined
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedVariable


translateRecordFieldSpec :: Spec
translateRecordFieldSpec = describe "translate record field test" $ do
  it "first record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id <: nil
          _ <- allocateLocalVariable "object" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)))
        ty `shouldBe` TInt

  it "second record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "y")
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt), ("y", TString)] <: #id @= id <: nil
          _ <- allocateLocalVariable "object" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const (F.wordSize @FrameMock))))
        ty `shouldBe` TString

  it "type synonym for record type" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id <: nil
              nameTy = TName (dummyRealLocated "record")
          insertType "record" recordTy
          _ <- allocateLocalVariable "object" True nameTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)))
        ty `shouldBe` TInt

  it "not record type" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "x")
        result = runEff $ do
          _ <- allocateLocalVariable "object" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedRecordType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedRecordType

  it "missing record field" $ do
    let ast = T.expToLExp $ T.Var (T.RecField (T.Id "object") "z")
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt), ("y", TString)] <: #id @= id <: nil
          _ <- allocateLocalVariable "object" True recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordField: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMissingRecordField


translateArrayIndexSpec :: Spec
translateArrayIndexSpec = describe "translate array index test" $ do
  it "array index x[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x[1 + 1]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Op (T.Int 1) T.Plus (T.Int 1)))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.BinOp IR.Plus (IR.Const 1) (IR.Const 1)) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x.y[0]" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.RecField (T.Id "x") "y") (T.Int 0))
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TString <: #id @= id1 <: nil
              recordTy = TRecord $ #map @= [("y", arrayTy)] <: #id @= id2 <: nil
          _ <- allocateLocalVariable "x" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TString

  it "type synonym for array type" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
              nameTy = TName (dummyRealLocated "array")
          insertType "array" arrayTy
          _ <- allocateLocalVariable "x" True nameTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) (IR.BinOp IR.Mul (IR.Const 0) (IR.Const (F.wordSize @FrameMock)))))
        ty `shouldBe` TInt

  it "array index x['hoge']" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.String "hoge"))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "array type expected" $ do
    let ast = T.expToLExp $ T.Var (T.ArrayIndex (T.Id "x") (T.Int 0))
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedArrayType" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedArrayType

translateBinOpSpec :: Spec
translateBinOpSpec = describe "translate binop test" $ do
  it "0 + 0" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.BinOp IR.Plus (IR.Const 0) (IR.Const 0))
        ty `shouldBe` TInt

  it "'hoge' + 1" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Plus (T.Int 0)
        result = runEff  $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "x + x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "x"))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "x == x (array)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "x"))
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
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
      Left (L _ e) -> e `shouldSatisfy` isNotDeterminedNilType

  it "nil == x (record)" $ do
    let ast = T.expToLExp $ T.Op T.Nil T.Eq (T.Var (T.Id "x"))
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldBe` IR.CJump IR.Eq (IR.Const 0) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock)))) true false
        ty `shouldBe` TInt
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "'hoge' == 'hoge'" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Eq (T.String "hoge")
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldSatisfy` expP true false
        ty `shouldBe` TInt
        where
          expP true false (IR.CJump IR.Ne (IR.Call (IR.Name _) [IR.Name _, IR.Name _]) (IR.Const 0) true' false') = true == true' && false == false'
          expP _ _ _ = False
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "'hoge' <> 'hoge'" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.NEq (T.String "hoge")
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((Cx genstm, ty), _) -> do
        let (true, false) = fetchTwoLabel
        genstm true false `shouldSatisfy` expP true false
        ty `shouldBe` TInt
        where
          expP true false (IR.CJump IR.Eq (IR.Call (IR.Name _) [IR.Name _, IR.Name _]) (IR.Const 0) true' false') = true == true' && false == false'
          expP _ _ _ = False
      Right ret -> expectationFailure $ "should return Cx, but got " ++ show ret

  it "x == y (array == record)" $ do
    let ast = T.expToLExp $ T.Op (T.Var (T.Id "x")) T.Eq (T.Var (T.Id "y"))
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id1 <: nil
              arrayTy = TArray  $ #range @= TInt <: #id @= id2 <: nil
          _ <- allocateLocalVariable "x" True recordTy
          _ <- allocateLocalVariable "y" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedType

  it "0 + (0 == 0)" $ do
    let ast = T.expToLExp $ T.Op (T.Int 0) T.Plus (T.Op (T.Int 0) T.Eq (T.Int 0))
        result = runEff $ do
          translateExp ast
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
        result = runEff $ do
          translateExp ast
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
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedExpression: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedExpression

  it "'hoge' < 'hoge'" $ do
    let ast = T.expToLExp $ T.Op (T.String "hoge") T.Lt (T.String "hoge")
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType


translateIfElseSpec :: Spec
translateIfElseSpec = describe "translate if-else test" $ do
  it "if 0 == 0 then 1 else 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 1) (Just (T.Int 0))
        result = runEff $ do
          translateExp ast
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
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
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
        result = runEff $ do
          translateExp ast
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
        result = runEff $ do
          translateExp ast
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
        result = runEff $ do
          translateExp ast
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
        result = runEff $ do
          translateExp ast
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
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType


translateIfNoElseSpec :: Spec
translateIfNoElseSpec = describe "translate if-no-else test" $ do
  it "if 0 == 0 then x := 0" $ do
    let ast = T.expToLExp $ T.If (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0)) Nothing
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
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
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
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
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray  $ #range @= TInt <: #id @= id <: nil
          _ <- allocateLocalVariable "x" True arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "if 0 then 0" $ do
    let ast = T.expToLExp $ T.If (T.Int 0) (T.Int 0) Nothing
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeInt: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedUnitType


translateRecordCreationSpec :: Spec
translateRecordCreationSpec = describe "translate record creation test" $ do
  it "type record = {}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const 0])) (IR.Temp _))) = True
          expP _ = False
          tyP (TRecord r) = r ^. #map == []
          tyP _ = False

  it "type record = {x: int}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const n])) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const 0))) (IR.Const 1))) (IR.Temp _))) = n == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = r ^. #map == [("x", TInt)]
          tyP _ = False

  it "type record = {x: int, y: string}; record {x = 1, y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1), T.FieldAssign "y" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt), ("y", TString)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const n])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const 0))) (IR.Const 1)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const i))) (IR.Name _)))) (IR.Temp _))) = n == 2 * F.wordSize @FrameMock && i == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = r ^. #map == [("x", TInt), ("y", TString)]
          tyP _ = False

  it "type record = {y: int, x: string}; record {y = 1, x = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "y" (T.Int 1), T.FieldAssign "x" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("y", TInt), ("x", TString)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const n])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const 0))) (IR.Const 1)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const i))) (IR.Name _)))) (IR.Temp _))) = n == 2 * F.wordSize @FrameMock && i == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = r ^. #map == [("y", TInt), ("x", TString)]
          tyP _ = False

  it "type record1 = {}; type record2 = {x: record1};  record2 {x: nil}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record2" [T.FieldAssign "x" T.Nil]
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let record1Ty = TRecord $ #map @= [] <: #id @= id1 <: nil
              record2Ty = TRecord $ #map @= [("x", record1Ty)] <: #id @= id2 <: nil
          insertType "record1" record1Ty
          insertType "record2" record2Ty
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const n])) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp _) (IR.Const 0))) (IR.Const 0))) (IR.Temp _))) = n == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = case List.lookup "x" (r ^. #map) of
            Just (TRecord _) -> True
            _ -> False
          tyP _ = False

  it "type record = {x: int}; record {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" []
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMissingRecordFieldInConstruction

  it "type record = {}; record {x = 1}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.Int 1)]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExtraRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExtraRecordFieldInConstruction

  it "type record = {x: int}; record {y = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "y" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MissingRecordFieldInConstruction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMissingRecordFieldInConstruction

  it "type record = {x: int}; record {x = 'hoge'}" $ do
    let ast = T.expToLExp $ T.RecordCreate "record" [T.FieldAssign "x" (T.String "hoge")]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [("x", TInt)] <: #id @= id <: nil
          insertType "record" recordTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypeForRecordField: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedTypeForRecordField

  it "type myint = int; myint {}" $ do
    let ast = T.expToLExp $ T.RecordCreate "myint" []
        result = runEff $ do
          insertType "myint" TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedRecordType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedRecordType


translateArrayCreationSpec :: Spec
translateArrayCreationSpec = describe "translate array creation test" $ do
  it "type array = array of int; array [0] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) (T.Int 1)
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          insertType "array" arrayTy
          translateExp ast
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

  it "type record = {}; type array = array of record; array [0] of nil" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) T.Nil
        result = runEff $ do
          id1 <- getUniqueEff #id
          id2 <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [] <: #id @= id1 <: nil
              arrayTy = TArray $ #range @= recordTy <: #id @= id2 <: nil
          insertType "record" recordTy
          insertType "array" arrayTy
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy`isRecordArray
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp _) (IR.Call (IR.Name _) [IR.Const 0, IR.Const 0])) (IR.Temp _))) = True
          expP _ = False
          isRecordArray (TArray r) = case r ^. #range of
            TRecord _ -> True
            _ -> False
          isRecordArray _ = False

  it "type myint = int; myint [0] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "myint" (T.Int 0) (T.Int 1)
        result = runEff $ do
          insertType "myint" TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedArrayType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedArrayType

  it "type array = array of int; array [0] of 'hoge'" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.Int 0) (T.String "hoge")
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          insertType "array" arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedType

  it "type array = array of int; array ['hoge'] of 0" $ do
    let ast = T.expToLExp $ T.ArrayCreate "array" (T.String "hoge") (T.Int 0)
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          insertType "array" arrayTy
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType


translateWhileLoopSpec :: Spec
translateWhileLoopSpec = describe "translate while loop test" $ do
  it "while 0 == 0 do x := 0" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0))
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
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
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "while 0 == 0 do 0" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedUnitType:" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedUnitType


translateForLoopSpec :: Spec
translateForLoopSpec = describe "translate for loop test" $ do
  it "for i := 1 to 2 do x := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) (T.Assign (T.Id "x") (T.Int 3))
        result = runEff $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp ast
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
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedUnitType:" ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedUnitType

  it "for i := 'hoge' to 2 do y := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.String "hoge") (T.Int 2) (T.Assign (T.Id "x") (T.Int 3))
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType"
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "for i := 1 to 'hoge' do x := 3" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.String "hoge") (T.Assign (T.Id "x") (T.Int 3))
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedIntType"
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType


translateBreakSpec :: Spec
translateBreakSpec = describe "translate break test" $ do
  it "while 0 == 0 do break" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) T.Break
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body done) (IR.Seq (IR.Label body') (IR.Seq (IR.Jump (IR.Name done') _) (IR.Seq (IR.Jump (IR.Name test') _) (IR.Label done''))))))) = test == test' && body == body' && done == done' && done == done''
          expP _ = False

  it "while 0 == 0 do (while 0 == 0 do break)" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) T.Break)
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.Label test1) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body1 done1) (IR.Seq (IR.Label body1') (IR.Seq (IR.Seq (IR.Label test2) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body2 done2) (IR.Seq (IR.Label body2') (IR.Seq (IR.Jump (IR.Name done2') _) (IR.Seq (IR.Jump (IR.Name test2') _) (IR.Label done2'')))))) (IR.Seq (IR.Jump (IR.Name test1') _) (IR.Label done1'))))))) = test1 == test1' && body1 == body1' && done1 == done1' && test2 == test2' && body2 == body2' && done2 == done2' && done2 == done2''
          expP _ = False

  it "for i := 1 to 3 do break" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) T.Break
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.Move (IR.Temp r) (IR.Const 1)) (IR.Seq (IR.Move (IR.Temp ul) (IR.Const 2)) (IR.Seq (IR.Label loop) (IR.Seq (IR.CJump IR.Le (IR.Temp r') (IR.Temp ul') body done) (IR.Seq (IR.Label body') (IR.Seq (IR.Jump (IR.Name done'') _) (IR.Seq (IR.Move (IR.Temp r'') (IR.BinOp IR.Plus (IR.Temp r''') (IR.Const 1))) (IR.Seq (IR.Jump (IR.Name loop') _) (IR.Label done')))))))))) = r == r' && r == r'' && r == r''' && ul == ul' && body == body' && done == done' && done == done'' && loop == loop'
          expP _ = False

  it "break" $ do
    let ast = T.expToLExp T.Break
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return BreakOutsideLoop: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isBreakOutsideLoop

  it "(for i := 1 to 3 do x := 2, break)" $ do
    let ast = T.expToLExp $ T.Seq [T.For "i" False (T.Int 1) (T.Int 2) (T.Assign (T.Id "x") (T.Int 3)), T.Break]
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return BreakOutsideLoop: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isBreakOutsideLoop


translateFunApplySpec :: Spec
translateFunApplySpec = describe "translate fun application test" $ do
  it "f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertVar "f" . Fun $ #label @= label <: #parent @= parent <: #domains @= [] <: #codomain @= TNil <: nil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TNil
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp (Temp (Unique _))])) = True
          expP _ = False

  it "f(1)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Int 0]
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertVar "f" . Fun $ #label @= label <: #parent @= parent <: #domains @= [TInt] <: #codomain @= TNil <: nil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TNil
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp (Temp (Unique _)), IR.Const 0])) = True
          expP _ = False

  it "type record = {}; f: record -> (); f(nil)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Nil]
        result = runEff $ do
          id <- getUniqueEff #id
          let recordTy = TRecord $ #map @= [] <: #id @= id <: nil
          insertType "record" recordTy
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertVar "f" . Fun $ #label @= label <: #parent @= parent <: #domains @= [recordTy] <: #codomain @= TNil <: nil
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TNil
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp (Temp (Unique _)), IR.Const 0])) = True
          expP _ = False

  it "f: int -> (); f('hoge')" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.String "hoge"]
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertVar "f" . Fun $ #label @= label <: #parent @= parent <: #domains @= [TInt] <: #codomain @= TNil <: nil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedTypes

  it "f: () -> (); f(0)" $ do
    let ast = T.expToLExp $ T.FunApply "f" [T.Int 0]
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertVar "f" . Fun $ #label @= label <: #parent @= parent <: #domains @= [] <: #codomain @= TNil <: nil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedTypes

  it "f: int -> (); f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          label <- newLabel
          parent <- fetchCurrentLevelEff
          insertVar "f" . Fun $ #label @= label <: #parent @= parent <: #domains @= [TInt] <: #codomain @= TNil <: nil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedTypes: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedTypes

  it "var f := (); f()" $ do
    let ast = T.expToLExp $ T.FunApply "f" []
        result = runEff $ do
          _ <- allocateLocalVariable "f" False TNil
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedFunction: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedFunction


translateAssignSpec :: Spec
translateAssignSpec = describe "translate assgin test" $ do
  it "var x int; x := 0" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "x") (T.Int 0)
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Move _ (IR.Const 0))) = True
          expP _ = False

  it "var x int; var y: unit; y := (x := 0)" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "y") (T.Assign (T.Id "x") (T.Int 0))
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          _ <- allocateLocalVariable "y" False TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Move (IR.Temp _) (IR.Const 0))) = True
          expP _ = False

  it "var x string; x := 0" $ do
    let ast = T.expToLExp $ T.Assign (T.Id "x") (T.Int 0)
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TString
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedType


translateSeqSpec :: Spec
translateSeqSpec = describe "translate seq test" $ do
  it "()" $ do
    let ast = T.expToLExp $ T.Seq []
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        ty `shouldBe` TUnit

  it "(1)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1]
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.Const 1)) = True
          expP _ = False

  it "(x := 0)" $ do
    let ast = T.expToLExp $ T.Seq [T.Assign (T.Id "x") (T.Int 0)]
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Move (IR.Temp _) (IR.Const 0))) = True
          expP _ = False

  it "(1, 2)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Int 2]
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        where
          expP (Ex (IR.ESeq (IR.Exp (IR.Const 1)) (IR.Const 2))) = True
          expP _ = False

  it "(1, x := 2)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Assign (T.Id "x") (T.Int 2)]
        result = runEff $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        where
          expP (Nx (IR.Seq (IR.Exp (IR.Const 1)) (IR.Move (IR.Temp _) (IR.Const 2)))) = True
          expP _ = False

  it "(1, ())" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Seq []]
        result = runEff $ translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Nx (IR.Seq (IR.Exp (IR.Const 1)) (IR.Exp (IR.Const 0)))
        ty `shouldBe` TUnit

translateLetSpec :: Spec
translateLetSpec = describe "translate let test" $ do
  it "let in 0" $ do
    let ast = T.expToLExp $ T.Let [] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt
        fragments `shouldBe` []

  it "let in ()" $ do
    let ast = T.expToLExp $ T.Let [] (T.Seq [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldBe` Nx (IR.Exp (IR.Const 0))
        ty `shouldBe` TUnit
        fragments `shouldBe` []

  it "let in 1 == 2" $ do
    let ast = T.expToLExp $ T.Let [] (T.Op (T.Int 1) T.Eq (T.Int 2))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldBe` []
        where
          expP (Cx genstm) =
            let (true, false) = fetchTwoLabel in
            case genstm true false of
              IR.CJump IR.Eq (IR.Const 1) (IR.Const 2) true' false' -> true == true' && false == false'
              _ -> False
          expP _ = False

  it "let var x: int := 0 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 0)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldBe` []
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp r) (IR.Const 0)) (IR.Temp r'))) = r == r'
          expP _ = False

  it "let var x := 0 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Int 0)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldBe` []
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp r) (IR.Const 0)) (IR.Temp r'))) = r == r'
          expP _ = False

  it "let var x := () in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Seq [])] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        fragments `shouldBe` []
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp r) (IR.Const 0)) (IR.Temp r'))) = r == r'
          expP _ = False

  it "let var x := 0 in 1 == 2" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Int 0)] (T.Op (T.Int 1) T.Eq (T.Int 2))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldBe` []
        where
          expP (Cx genstm) =
            let (true, false) = fetchTwoLabel in
            case genstm true false of
              IR.Seq (IR.Move (IR.Temp _) (IR.Const 0)) (IR.CJump IR.Eq (IR.Const 1) (IR.Const 2) true' false') -> true == true' && false == false'
              _ -> False
          expP _ = False

  it "let var x: int := 0 in x := 1" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 0)] (T.Assign (T.Id "x") (T.Int 1))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        fragments `shouldBe` []
        where
          expP (Nx (IR.Seq (IR.Move (IR.Temp r) (IR.Const 0)) (IR.Move (IR.Temp r') (IR.Const 1)))) = r == r'
          expP _ = False

  it "let var x: int := let var y: int := 0 in y in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Let [T.VarDec "y" False (Just "int") (T.Int 0)] (T.Var (T.Id "y")))] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldBe` []
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp r) (IR.ESeq (IR.Move (IR.Temp r') (IR.Const 0)) (IR.Temp r''))) (IR.Temp r'''))) = r' == r'' && r == r'''
          expP _ = False

  it "let var x: int = 1; var x: int = 2 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 1), T.VarDec "x" False (Just "int") (T.Int 2)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldBe` []
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp _) (IR.Const 1)) (IR.Move (IR.Temp r) (IR.Const 2))) (IR.Temp r'))) = r == r'
          expP _ = False

  it "let var: int x = 1; var y: int = let var x: int := 2 in x in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 1), T.VarDec "y" False (Just "int") (T.Let [T.VarDec "x" False (Just "int") (T.Int 2)] (T.Var (T.Id "x")))] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldBe` []
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp r) (IR.Const 1)) (IR.Move (IR.Temp _) (IR.ESeq (IR.Move (IR.Temp r') (IR.Const 2)) (IR.Temp r'')))) (IR.Temp r'''))) = r == r''' && r' == r''
          expP _ = False

  it "let type record = {} in record {}" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "record" (T.RecordType [])] (T.RecordCreate "record" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        fragments `shouldBe` []
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp r) (IR.Call (IR.Name _) [IR.Const 0])) (IR.Temp r'))) = r == r'
          expP _ = False
          tyP (TRecord r) = r ^. #map == []
          tyP _ = False

  it "let type record = {y: int, x: string} in record {y: 0, x: 'hoge'}" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "record" (T.RecordType [T.Field "y" False "int", T.Field "x" False "string"])] (T.RecordCreate "record" [T.FieldAssign "y" (T.Int 0), T.FieldAssign "x" (T.String "hoge")])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        fragments `shouldSatisfy` fragmentsP
        where
          expP (Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp r) (IR.Call (IR.Name _) [IR.Const n])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp r') (IR.Const 0))) (IR.Const 0)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp r'') (IR.Const n'))) (IR.Name _)))) (IR.Temp r'''))) = r == r' && r == r'' && r == r''' && n == 2 * F.wordSize @FrameMock && n' == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = r ^. #map == [("y", TInt), ("x", TString)]
          tyP _ = False
          fragmentsP [F.String _ s] = s == "hoge"
          fragmentsP _ = False

  it "let function f(x: int): int = x in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Var (T.Id "x"))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp _, IR.Const 1])) = True
          expP _ = False
          bodyP [F.Proc r] = case r ^. #body of
            IR.Move (IR.Temp rv) (IR.Temp (Temp _ )) -> rv == F.rv @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r] = case r ^. #frame of
            FrameMock r -> case r ^. #formals of
              [InFrame 0, InReg _] -> r ^. #numberOfLocals == 0
              _ -> False
          frameP _ = False

  it "let function f(x: int): int = let var y: int = 1 in x + y in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Let [T.VarDec "y" True (Just "int") (T.Int 1)] (T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "y"))))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp _, IR.Const 1])) = True
          expP _ = False
          bodyP [F.Proc r] = case r ^. #body of
            IR.Move (IR.Temp rv) (IR.ESeq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 1)) (IR.BinOp IR.Plus (IR.Temp (Temp _)) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp'))))) -> rv == F.rv @FrameMock && fp == F.fp @FrameMock && fp' == F.fp @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r] = case r ^. #frame of
            FrameMock r -> case r ^. #formals of
              [InFrame 0, InReg _] -> r ^. #numberOfLocals == 1
              _ -> False
          frameP _ = False

  it "let function f(x: int): int = let function g(y: int): int = x + y in g(0) in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" True "int"] (Just "int") (T.Let [T.FunDec "g" [T.Field "y" False "int"] (Just "int") (T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "y")))] (T.FunApply "g" [T.Int 0]))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp fp, IR.Const 1])) = fp == F.fp @FrameMock
          expP _ = False
          bodyP [F.Proc r1, F.Proc r2] = case (r1 ^. #body, r2 ^. #body) of
            (IR.Move (IR.Temp rv1) (IR.Call (IR.Name _) [IR.Temp fp1, IR.Const 0]), IR.Move (IR.Temp rv2) (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp2))))) (IR.Temp (Temp _)))) -> rv1 == F.rv @FrameMock && rv2 == F.rv @FrameMock && fp1 == F.fp @FrameMock && fp2 == F.fp @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r1, F.Proc r2] = case (r1 ^. #frame, r2 ^. #frame) of
            (FrameMock r1, FrameMock r2) -> case (r1 ^. #formals, r2 ^. #formals) of
                  ([InFrame 0, InFrame 4], [InFrame 0, InReg _]) -> r1 ^. #numberOfLocals == 0 && r2 ^. #numberOfLocals == 0
                  _ -> False
          frameP _ = False

  it "let type a = record{}; function f(): a = nil in f()" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType []), T.FunDec "f" [] (Just "a") T.Nil] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp fp])) = fp == F.fp @FrameMock
          expP _ = False
          tyP (TRecord r) = r ^. #map == []
          tyP _ = False
          bodyP [F.Proc r] = case r ^. #body of
            IR.Move (IR.Temp rv) (IR.Const 0) -> rv == F.rv @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r] = case r ^. #frame of
            FrameMock r -> case r ^. #formals of
              [InFrame 0] -> r ^. #numberOfLocals == 0
              _ -> False
          frameP _ = True

  it "let type myint = int; var a: myint = 1; function f(): myint = a; in f()" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "myint" (T.TypeId "int"), T.VarDec "x" True (Just "myint") (T.Int 1), T.FunDec "f" [] (Just "myint") (T.Var (T.Id "x"))] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp fp))) (IR.Const 1)) (IR.Call (IR.Name _) [IR.Temp fp']))) = fp == F.fp @FrameMock && fp' == F.fp @FrameMock
          expP _ = False
          bodyP [F.Proc r] = case r ^. #body of
            IR.Move (IR.Temp rv) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp))))) -> rv == F.rv @FrameMock && fp == F.fp @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r] = case r ^. #frame of
            FrameMock r -> case r ^. #formals of
              [InFrame 0] -> r ^. #numberOfLocals == 0
              _ -> False
          frameP _ = False

  it "let var f: int = 1; function f(): int = 1 in f()" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "f" False (Just "int") (T.Int 1), T.FunDec "f" [] (Just "int") (T.Int 1)] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp _) (IR.Const 1)) (IR.Call (IR.Name _) [IR.Temp fp]))) = fp == F.fp @FrameMock
          expP _ = False
          bodyP [F.Proc r] = case r ^. #body of
            IR.Move (IR.Temp rv) (IR.Const 1) -> rv == F.rv @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r] = case r ^. #frame of
            FrameMock r -> case r ^. #formals of
              [InFrame 0] -> r ^. #numberOfLocals == 0
              _ -> False
          frameP _ = False

  it "let function f(): int = 1; var f: int = 1 in f" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] (Just "int") (T.Int 1), T.VarDec "f" False (Just "int") (T.Int 1)] (T.Var (T.Id "f"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TInt
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp r) (IR.Const 1)) (IR.Temp r'))) = r == r'
          expP _ = False
          bodyP [F.Proc r] = case r ^. #body of
            IR.Move (IR.Temp rv) (IR.Const 1) -> rv == F.rv @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r] = case r ^. #frame of
            FrameMock r -> case r ^. #formals of
              [InFrame 0] -> r ^. #numberOfLocals == 0
              _ -> False
          frameP _ = False

  it "let function f() = g(); function g() = f() in f()" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] Nothing (T.FunApply "g" []), T.FunDec "g" [] Nothing (T.FunApply "f" [])] (T.FunApply "g" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TUnit
        fragments `shouldSatisfy` bodyP
        fragments `shouldSatisfy` frameP
        where
          expP (Ex (IR.Call (IR.Name _) [IR.Temp fp])) = fp == F.fp @FrameMock
          expP _ = False
          bodyP [F.Proc r1, F.Proc r2] = case (r1 ^. #body, r2 ^. #body) of
            (IR.Move (IR.Temp rv1) (IR.Call (IR.Name _) [IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp1))]), (IR.Move (IR.Temp rv2) (IR.Call (IR.Name _) [IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp2))]))) -> rv1 == F.rv @FrameMock && fp1 == F.fp @FrameMock && rv2 == F.rv @FrameMock && fp2 == F.fp @FrameMock
            _ -> False
          bodyP _ = False
          frameP [F.Proc r1, F.Proc r2] = case (r1 ^. #frame, r2 ^. #frame) of
            (FrameMock r1, FrameMock r2) -> case (r1 ^. #formals, r2 ^. #formals) of
              ([InFrame 0], [InFrame 0]) -> r1 ^. #numberOfLocals == 0 && r2 ^. #numberOfLocals == 0
              _ -> False
          frameP _ = False

  it "let type a = {a: b}; type b = {b: a}; var x: a = nil in x" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType [T.Field "a" False "b"]), T.TypeDec "b" (T.RecordType [T.Field "b" False "a"]), T.VarDec "x" False (Just "a") T.Nil] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp r) (IR.Const 0)) (IR.Temp r'))) = r == r'
          expP _ = False
          tyP (TRecord r) = case r ^. #map of
            [("a", TName b)] -> b == dummyRealLocated "b"
            _ -> False
          tyP _ = False

  it "let type intlist = {hd: int, tl: intlist}; var list:intlist := intlist {hd: 0, tl: nil} in list" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "intlist" (T.RecordType [T.Field "hd" False "int", T.Field "tl" False "intlist"]), T.VarDec "list" False (Just "intlist") (T.RecordCreate "intlist" [T.FieldAssign "hd" (T.Int 0), T.FieldAssign "tl" T.Nil])] (T.Var (T.Id "list"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` expP
        ty `shouldSatisfy` tyP
        where
          expP (Ex (IR.ESeq (IR.Move (IR.Temp x) (IR.ESeq (IR.Seq (IR.Move (IR.Temp r) (IR.Call (IR.Name _) [IR.Const n])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp r') (IR.Const 0))) (IR.Const 0)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp r'') (IR.Const n'))) (IR.Const 0)))) (IR.Temp r'''))) (IR.Temp x'))) = x == x' && r == r' && r == r'' && r == r''' && n == 2 * F.wordSize @FrameMock && n' == F.wordSize @FrameMock
          expP _ = False
          tyP (TRecord r) = case r ^. #map of
            [("hd", TInt), ("tl", TName intlist)] -> intlist == dummyRealLocated "intlist"
            _ -> False
          tyP _ = False

  it "let var x: myint := 0 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "myint") (T.Int 0)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UnknownType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUnknownType

  it "let x := let y := 0 in y in y" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False Nothing (T.Let [T.VarDec "y" False Nothing (T.Int 0)] (T.Var (T.Id "y")))] (T.Var (T.Id "y"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UndefinedVariable: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUndefinedVariable

  it "let type a = b; type b = c; type c = a in 0" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.TypeId "b"), T.TypeDec "b" (T.TypeId "c"), T.TypeDec "c" (T.TypeId "a")] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return InvalidRecTypeDeclaration: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isInvalidRecTypeDeclaration

  it "type a = {a: b}; var x = 0; type b = {b: a} in x" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType [T.Field "a" False "b"]), T.VarDec "x" False Nothing (T.Int 0), T.TypeDec "b" (T.RecordType [T.Field "b" False "a"])] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UnknownType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUnknownType

  it "let function f() = g(); var x := 0; function g() = f() in f()" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] Nothing (T.FunApply "g" []), T.VarDec "x" False Nothing (T.Int 0), T.FunDec "g" [] Nothing (T.FunApply "f" [])] (T.FunApply "g" [])
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UndefinedVariale: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUndefinedVariable

  it "let var x: int = 'hoge' in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.String "hoge")] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return ExpectedType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isExpectedType

  it "let function f(x: hoge): int = 0 in 0" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "hoge"] (Just "int") (T.Int 0)] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return UnknownType: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUnknownType

  it "let function f(): int = 0; function f(): int = 0 in 0" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] (Just "int") (T.Int 0), T.FunDec "f" [] (Just "int") (T.Int 0)] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MultiDeclaredName: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMultiDeclaredName

  it "let type a = int; type a = string in 0" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.TypeId "int"), T.TypeDec "a" (T.TypeId "int")] (T.Int 0)
        result = runEff $ do
          translateExp ast
    case result of
      Right ret -> expectationFailure $ "should return MultiDeclaredName: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isMultiDeclaredName


runEff :: Eff '[
        ("typeEnv" >: State TEnv)
      , ("varEnv" >: State (VEnv FrameMock))
      , ("nestingLevel" >: NestingLevelEff FrameMock)
      , ("breakpoint" >: BreakPointEff)
      , ("fragment" >: FragmentEff FrameMock)
      , ("temp" >: UniqueEff)
      , ("label" >: UniqueEff)
      , ("id" >: UniqueEff)
      , ("translateError" >: EitherEff (RealLocated TranslateError))
      ] a
  -> Either (RealLocated TranslateError) (a, [F.ProgramFragment FrameMock])
runEff = leaveEff . runTranslateEffWithNewLevel
