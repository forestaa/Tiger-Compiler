module Compiler.Frontend.Language.Tiger.Semant.Translate.LoopSpec (spec) where

import Compiler.Frontend.FrameMock (FrameMock)
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp)
import Compiler.Frontend.Language.Tiger.Semant (translateExp)
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level (fetchCurrentLevelEff)
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (allocateLocalVariableAndInsertType, insertFun, isTranslateErrorAndBreakOutsideLoop, isTypeCheckErrorAndExpectedIntType, isTypeCheckErrorAndExpectedUnitType, runEff)
import Compiler.Frontend.SrcLoc (RealLocated (L), dummyRealLocated)
import Compiler.Intermediate.Frame qualified as F (fp, wordSize)
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U (newLabel)
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthTemp)
import RIO
import RIO.Text qualified as Text (unpack)
import Test.Hspec

spec :: Spec
spec = do
  translateWhileLoopSpec
  translateForLoopSpec
  translateBreakSpec

translateWhileLoopSpec :: Spec
translateWhileLoopSpec = describe "translate while loop test" $ do
  it "while 0 == 0 do x := 0" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.Assign (T.Id "x") (T.Int 0))
        result = runEff $ do
          allocateLocalVariableAndInsertType "x" True TInt
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let done = newNthLabel 1
            test = newNthLabel 2
            body = newNthLabel 3
        exp `shouldBe` Nx (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 0)) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done))))))
        ty `shouldBe` TUnit

  it "f: () -> (); while 0 == 0 do f()" $ do
    let ast = T.expToLExp $ T.While (T.Op (T.Int 0) T.Eq (T.Int 0)) (T.FunApply "f" [])
        result = runEff $ do
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let done = newNthLabel 2
            test = newNthLabel 3
            body = newNthLabel 4
            f = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Eq (IR.Const 0) (IR.Const 0) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Exp (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock)])) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done))))))
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            ul = newNthTemp 1
            done = newNthLabel 1
            test = newNthLabel 2
            body = newNthLabel 3
        exp `shouldBe` Nx (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Move (IR.Temp ul) (IR.Const 2)) (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Le (IR.Temp t) (IR.Temp ul) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Move (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp @FrameMock)))) (IR.Const 3)) (IR.Seq (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done)))))))))
        ty `shouldBe` TUnit

  it "f: () -> (); for i := 1 to 2 do f()" $ do
    let ast = T.expToLExp $ T.For "i" False (T.Int 1) (T.Int 2) (T.FunApply "f" [])
        result = runEff $ do
          label <- U.newLabel
          parent <- fetchCurrentLevelEff
          insertFun "f" label parent [] TUnit
          translateExp ast
    case result of
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
      Right (((exp, ty), _), _) -> do
        let t = newNthTemp 0
            ul = newNthTemp 1
            done = newNthLabel 2
            test = newNthLabel 3
            body = newNthLabel 4
            f = newNthLabel 1
        exp `shouldBe` Nx (IR.Seq (IR.Move (IR.Temp t) (IR.Const 1)) (IR.Seq (IR.Move (IR.Temp ul) (IR.Const 2)) (IR.Seq (IR.Label test) (IR.Seq (IR.CJump IR.Le (IR.Temp t) (IR.Temp ul) body done) (IR.Seq (IR.Label body) (IR.Seq (IR.Exp (IR.Call (IR.Name f) [IR.Temp (F.fp @FrameMock)])) (IR.Seq (IR.Move (IR.Temp t) (IR.BinOp IR.Plus (IR.Temp t) (IR.Const 1))) (IR.Seq (IR.Jump (IR.Name test) [test]) (IR.Label done)))))))))
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
      Left (L _ e) -> expectationFailure . Text.unpack $ textDisplay e
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
