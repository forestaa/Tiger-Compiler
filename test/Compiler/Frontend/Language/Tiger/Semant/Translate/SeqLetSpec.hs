module Compiler.Frontend.Language.Tiger.Semant.Translate.SeqLetSpec (spec) where

import Compiler.Frontend.FrameMock (AccessMock (..), FrameMock (formals, localVariables))
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant (translateExp)
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (allocateLocalVariableAndInsertType, isTypeCheckErrorAndExpectedType, isTypeCheckErrorAndInvalidRecTypeDeclaration, isTypeCheckErrorAndMultiDeclaredName, isTypeCheckErrorAndUndefinedVariable, isTypeCheckErrorAndUnknownType, runEff)
import Compiler.Frontend.SrcLoc (RealLocated (L), dummyRealLocated)
import Compiler.Intermediate.Frame qualified as F (Procedure (body, frame), ProgramFragments (fragments), StringFragment (text), fp, rv, wordSize)
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U (externalLabel)
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthNamedLabel, newNthTemp, newNthUnique)
import Compiler.Utils.Maybe ()
import RIO
import RIO.List.Partial qualified as List ((!!))
import RIO.Text qualified as Text (unpack)
import Test.Hspec

spec :: Spec
spec = do
  translateSeqSpec
  translateLetSpec

translateSeqSpec :: Spec
translateSeqSpec = describe "translate seq test" $ do
  it "()" $ do
    let ast = T.expToLExp $ T.Seq []
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        ty `shouldBe` TUnit

  it "(1)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1]
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.Const 1)
        ty `shouldBe` TInt

  it "(x := 0)" $ do
    let ast = T.expToLExp $ T.Seq [T.Assign (T.Id "x") (T.Int 0)]
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
        exp `shouldBe` Nx (IR.Move (IR.Temp t) (IR.Const 0))
        ty `shouldBe` TUnit

  it "(1, 2)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Int 2]
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        exp `shouldBe` Ex (IR.ESeq (IR.Exp (IR.Const 1)) (IR.Const 2))
        ty `shouldBe` TInt

  it "(1, x := 2)" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Assign (T.Id "x") (T.Int 2)]
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" False TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
        exp `shouldBe` Nx (IR.Seq (IR.Exp (IR.Const 1)) (IR.Move (IR.Temp t) (IR.Const 2)))
        ty `shouldBe` TUnit

  it "(1, ())" $ do
    let ast = T.expToLExp $ T.Seq [T.Int 1, T.Seq []]
        result = runEff $ translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let in ()" $ do
    let ast = T.expToLExp $ T.Let [] (T.Seq [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Nx (IR.Exp (IR.Const 0))
        ty `shouldBe` TUnit
        fragments.fragments `shouldBe` []

  it "let in 1 == 2" $ do
    let ast = T.expToLExp $ T.Let [] (T.Op (T.Int 1) T.Eq (T.Int 2))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        exp `shouldBe` Cx (\true false -> IR.CJump IR.Eq (IR.Const 1) (IR.Const 2) true false)
        ty `shouldBe` TInt
        fragments.fragments `shouldBe` []

  it "let var x: int := 0 in x" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "x" False (Just "int") (T.Int 0)] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
            malloc = U.externalLabel "malloc"
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const 0])) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = []}
        fragments.fragments `shouldBe` []

  it "let type record = {y: int, x: string} in record {y: 0, x: 'hoge'}" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "record" (T.RecordType [T.Field "y" False "int", T.Field "x" False "string"])] (T.RecordCreate "record" [T.FieldAssign "y" (T.Int 0), T.FieldAssign "x" (T.String "hoge")])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
            malloc = U.externalLabel "malloc"
            l = newNthLabel 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.ESeq (IR.Seq (IR.Move (IR.Temp t) (IR.Call (IR.Name malloc) [IR.Const (2 * (F.wordSize @FrameMock))])) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 0))) (IR.Const 0)) (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Temp t) (IR.Const (F.wordSize @FrameMock)))) (IR.Name l)))) (IR.Temp t))
        ty `shouldBe` TRecord {id = id, map = [("y", TInt), ("x", TString)]}
        length fragments.fragments `shouldBe` 1
        (fragments.fragments List.!! 0).string.text `shouldBe` Just "hoge"

  it "let function f(x: int): int = x in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Var (T.Id "x"))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock), IR.Const 1])
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Temp t))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0, InReg t]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let function f(x: int): int = let var y: int = 1 in x + y in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" False "int"] (Just "int") (T.Let [T.VarDec "y" True (Just "int") (T.Int 1)] (T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "y"))))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
            f = newNthNamedLabel "f" 1
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock), IR.Const 1])
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.ESeq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 1)) (IR.BinOp IR.Plus (IR.Temp t) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))))))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0, InReg t]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 1
        (fragments.fragments List.!! 0).procedure.frame.localVariables `shouldBe` Just [InFrame (-4)]

  it "let function f(x: int): int = let function g(y: int): int = x + y in g(0) in f(1)" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [T.Field "x" True "int"] (Just "int") (T.Let [T.FunDec "g" [T.Field "y" False "int"] (Just "int") (T.Op (T.Var (T.Id "x")) T.Plus (T.Var (T.Id "y")))] (T.FunApply "g" [T.Int 0]))] (T.FunApply "f" [T.Int 1])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            g = newNthNamedLabel "g" 2
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock), IR.Const 1])
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 2
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.BinOp IR.Plus (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp (F.fp @FrameMock)))))) (IR.Temp t)))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0, InReg t]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0
        (fragments.fragments List.!! 1).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Call (IR.Name g) [IR.Temp (F.fp @FrameMock), IR.Const 0]))
        (fragments.fragments List.!! 1).procedure.frame.formals `shouldBe` Just [InFrame 0, InFrame (-4)]
        (fragments.fragments List.!! 1).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let type a = record{}; function f(): a = nil in f()" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType []), T.FunDec "f" [] (Just "a") T.Nil] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            id = newNthUnique 0
        exp `shouldBe` Ex (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock)])
        ty `shouldBe` TRecord {id = id, map = []}
        length fragments.fragments `shouldBe` 1
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Const 0))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let type myint = int; var a: myint = 1; function f(): myint = a; in f()" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "myint" (T.TypeId "int"), T.VarDec "x" True (Just "myint") (T.Int 1), T.FunDec "f" [] (Just "myint") (T.Var (T.Id "x"))] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 1)) (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock)]))
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp (F.fp @FrameMock)))))))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let var f: int = 1; function f(): int = 1 in f()" $ do
    let ast = T.expToLExp $ T.Let [T.VarDec "f" False (Just "int") (T.Int 1), T.FunDec "f" [] (Just "int") (T.Int 1)] (T.FunApply "f" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock)]))
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Const 1))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let function f(): int = 1; var f: int = 1 in f" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] (Just "int") (T.Int 1), T.VarDec "f" False (Just "int") (T.Int 1)] (T.Var (T.Id "f"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let t = newNthTemp 0
        exp `shouldBe` Ex (IR.ESeq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Temp t))
        ty `shouldBe` TInt
        length fragments.fragments `shouldBe` 1
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Const 1))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let function f() = g(); function g() = f() in f()" $ do
    let ast = T.expToLExp $ T.Let [T.FunDec "f" [] Nothing (T.FunApply "g" []), T.FunDec "g" [] Nothing (T.FunApply "f" [])] (T.FunApply "g" [])
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), fragments) -> do
        let f = newNthNamedLabel "f" 1
            g = newNthNamedLabel "g" 2
        exp `shouldBe` Ex (IR.Call (IR.Name g) [IR.Temp (F.fp @FrameMock)])
        ty `shouldBe` TUnit
        length fragments.fragments `shouldBe` 2
        (fragments.fragments List.!! 0).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Call (IR.Name g) [IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp (F.fp @FrameMock)))]))
        (fragments.fragments List.!! 0).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments List.!! 0).procedure.frame.numberOfLocals
          `shouldBe` Just 0
        (fragments.fragments List.!! 1).procedure.body `shouldBe` Just (IR.Move (IR.Temp (F.rv @FrameMock)) (IR.Call (IR.Name f) [IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp (F.fp @FrameMock)))]))
        (fragments.fragments List.!! 1).procedure.frame.formals `shouldBe` Just [InFrame 0]
        (fragments.fragments List.!! 1).procedure.frame.numberOfLocals `shouldBe` Just 0

  it "let type a = {a: b}; type b = {b: a}; var x: a = nil in x" $ do
    let ast = T.expToLExp $ T.Let [T.TypeDec "a" (T.RecordType [T.Field "a" False "b"]), T.TypeDec "b" (T.RecordType [T.Field "b" False "a"]), T.VarDec "x" False (Just "a") T.Nil] (T.Var (T.Id "x"))
        result = runEff $ do
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t0 = newNthTemp 0
            t1 = newNthTemp 1
            malloc = U.externalLabel "malloc"
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
