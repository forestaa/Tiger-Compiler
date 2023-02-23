module Compiler.Frontend.Language.Tiger.IntegrationSpec (spec) where

import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Exception (FrontendException (fromFrontendException, toFrontendException), SomeFrontendException (SomeFrontendException))
import Compiler.Frontend.FrameMock (AccessMock (InFrame, InReg), FrameMock (..), fp, isInFrame, isInRegister, rv)
import Compiler.Frontend.Language.Tiger (Tiger (Tiger))
import Compiler.Frontend.Language.Tiger.Samples (tigerTest, validTigerTests)
import Compiler.Frontend.Language.Tiger.Semant (SemantAnalysisError)
import Compiler.Frontend.Language.Tiger.TestUtils
import Compiler.Frontend.Lexer (ParserException (ParserException))
import Compiler.Frontend.SrcLoc (RealLocated (L))
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthNamedLabel, newNthTemp)
import Compiler.Utils.Maybe
import Control.Exception.Safe (Exception (toException), MonadCatch, MonadThrow, catch)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.ByteString.Lazy qualified as B
import Data.Extensible.Effect (liftEff, runEitherEff)
import Data.Extensible.Effect.Default (runIODef)
import RIO hiding (catch)
import RIO.List.Partial ((!!))
import RIO.Text qualified as T
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  validTestSpec
  invalidTestSpec
  complexTestSpec

validTestSpec :: Spec
validTestSpec = describe "valid integration test for tiger to translate" $ do
  it "test01.tig" $ do
    let testcase = tigerTest "test01.tig"
    res <- translateTest' testcase
    let temp0 = newNthTemp 0
        temp1 = newNthTemp 1
        initArrayLabel = newNthNamedLabel "initArray" 11
        tiger = newNthNamedLabel "tiger" 0
    res.main.procedure.body
      `shouldBe` Just
        ( IR.Exp
            ( ( IR.Move
                  (IR.Temp temp1)
                  ( ( IR.Move
                        (IR.Temp temp0)
                        ( IR.Call
                            (IR.Name initArrayLabel)
                            [IR.Const 10, IR.Const 0]
                        )
                    )
                      `IR.ESeq` (IR.Temp temp0)
                  )
              )
                `IR.ESeq` (IR.Temp temp1)
            )
        )
    res.main.procedure.frame.name `shouldBe` Just tiger
    res.main.procedure.frame.formals `shouldBe` Just [InFrame 0]
    res.main.procedure.frame.numberOfLocals `shouldBe` Just 2
    res.main.procedure.frame.head `shouldBe` Just (-4)
    res.fragments `shouldBe` []

  it "test02.tig" $ do
    let testcase = tigerTest "test02.tig"
    res <- translateTest' testcase
    let temp0 = newNthTemp 0
        temp1 = newNthTemp 1
        initArrayLabel = newNthNamedLabel "initArray" 11
        tiger = newNthNamedLabel "tiger" 0
    res.main.procedure.body
      `shouldBe` Just
        ( IR.Exp
            ( IR.Move
                (IR.Temp temp1)
                ( IR.Move
                    (IR.Temp temp0)
                    ( IR.Call
                        (IR.Name initArrayLabel)
                        [IR.Const 10, IR.Const 0]
                    )
                    IR.>>& IR.Temp temp0
                )
                IR.>>& IR.Temp temp1
            )
        )
    res.main.procedure.frame.name `shouldBe` Just tiger
    res.main.procedure.frame.formals `shouldBe` Just [InFrame 0]
    res.main.procedure.frame.numberOfLocals `shouldBe` Just 2
    res.main.procedure.frame.localVariables `shouldSatisfy` maybe False (all isInRegister)
    res.main.procedure.frame.head `shouldBe` Just (-4)
    res.fragments `shouldBe` []

  it "test03.tig" $ do
    let testcase = tigerTest "test03.tig"
    res <- translateTest' testcase
    let temp0 = newNthTemp 0
        temp1 = newNthTemp 1
        nobody = newNthLabel 11
        somebody = newNthLabel 13
        mallocLabel = newNthNamedLabel "malloc" 12
        tiger = newNthNamedLabel "tiger" 0
    res.main.procedure.body
      `shouldBe` Just
        ( IR.Exp
            ( IR.Move
                (IR.Temp temp1)
                ( IR.Move
                    (IR.Temp temp0)
                    ( IR.Call
                        (IR.Name mallocLabel)
                        [IR.Const 8]
                    )
                    IR.>> IR.Move
                      (IR.Mem (IR.BinOp IR.Plus (IR.Temp temp0) (IR.Const 0)))
                      (IR.Name nobody)
                    IR.>> IR.Move
                      (IR.Mem (IR.BinOp IR.Plus (IR.Temp temp0) (IR.Const 4)))
                      (IR.Const 1000)
                      IR.>>& IR.Temp temp0
                )
                IR.>>& IR.Move
                  (IR.Mem (IR.BinOp IR.Plus (IR.Temp temp1) (IR.Const 0)))
                  (IR.Name somebody)
                IR.>>& IR.Temp temp1
            )
        )
    res.main.procedure.frame.name `shouldBe` Just tiger
    res.main.procedure.frame.formals `shouldBe` Just [InFrame 0]
    res.main.procedure.frame.numberOfLocals `shouldBe` Just 2
    res.main.procedure.frame.localVariables `shouldSatisfy` maybe False (all isInRegister)
    res.main.procedure.frame.head `shouldBe` Just (-4)
    length res.fragments `shouldBe` 2
    (res.fragments !! 0).string.name `shouldBe` Just nobody
    (res.fragments !! 0).string.text `shouldBe` Just "\"Nobody\""
    (res.fragments !! 1).string.name `shouldBe` Just somebody
    (res.fragments !! 1).string.text `shouldBe` Just "\"Somebody\""

  it "test04.tig" $ do
    let testcase = tigerTest "test04.tig"
    res <- translateTest' testcase
    let temp0 = newNthTemp 0
        temp1 = newNthTemp 1
        label12 = newNthLabel 12
        label13 = newNthLabel 13
        label14 = newNthLabel 14
        nfactor = newNthNamedLabel "nfactor" 11
        tiger = newNthNamedLabel "tiger" 0
    res.main.procedure.body
      `shouldBe` Just
        ( IR.Exp (IR.Call (IR.Name nfactor) [IR.Temp fp, IR.Const 10])
        )
    res.main.procedure.frame.name `shouldBe` Just tiger
    res.main.procedure.frame.formals `shouldBe` Just [InFrame 0]
    res.main.procedure.frame.numberOfLocals `shouldBe` Just 0
    res.main.procedure.frame.head `shouldBe` Just (-4)
    length res.fragments `shouldBe` 1
    (res.fragments !! 0).procedure.body
      `shouldBe` Just
        ( IR.Move
            (IR.Temp rv)
            ( IR.CJump IR.Eq (IR.Temp temp0) (IR.Const 0) label12 label13
                IR.>> IR.Label label12
                IR.>> IR.Move (IR.Temp temp1) (IR.Const 1)
                IR.>> IR.Jump (IR.Name label14) [label14]
                IR.>> IR.Label label13
                IR.>> IR.Move
                  (IR.Temp temp1)
                  ( IR.BinOp
                      IR.Mul
                      (IR.Temp temp0)
                      ( IR.Call
                          (IR.Name nfactor)
                          [ IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp)),
                            IR.BinOp IR.Minus (IR.Temp temp0) (IR.Const 1)
                          ]
                      )
                  )
                IR.>> IR.Jump (IR.Name label14) [label14]
                IR.>> IR.Label label14
                  IR.>>& (IR.Temp temp1)
            )
        )
    (res.fragments !! 0).procedure.frame.name `shouldBe` Just nfactor
    (res.fragments !! 0).procedure.frame.formals `shouldBe` Just [InFrame 0, InReg temp0]
    (res.fragments !! 0).procedure.frame.numberOfLocals `shouldBe` Just 1
    res.main.procedure.frame.localVariables `shouldSatisfy` maybe False (all isInRegister)
    (res.fragments !! 0).procedure.frame.head `shouldBe` Just (-4)

  it "valid test cases" $ do
    res <- runExceptT (traverse (ExceptT . translateTest) validTigerTests)
    res `shouldSatisfy` isRight

invalidTestSpec :: Spec
invalidTestSpec = describe "invalid integration test cases for tiger to translate" $ do
  it "then and else type differ" $
    runErrorTranslateTest (tigerTest "test09.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "body of while isTypeCheckErrorAnd not unit" $
    runErrorTranslateTest (tigerTest "test10.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedUnitType)

  it "hi in for is not int" $
    runErrorTranslateTest (tigerTest "test11.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "incompatible comparison: lt" $
    runErrorTranslateTest (tigerTest "test13.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "incompatible comparison: eq" $
    runErrorTranslateTest (tigerTest "test14.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "if-then returns non unit" $
    runErrorTranslateTest (tigerTest "test15.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedUnitType)

  it "invalid recursive type" $
    runErrorTranslateTest (tigerTest "test16.tig") (`shouldSatisfy` isTypeCheckErrorAndInvalidRecTypeDeclaration)

  it "invalid recursive type: interrupted" $
    runErrorTranslateTest (tigerTest "test17.tig") (`shouldSatisfy` isTypeCheckErrorAndUnknownType)

  it "invalid recursive function: interrupted" $
    runErrorTranslateTest (tigerTest "test18.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (tigerTest "test19.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (tigerTest "test20.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "procedure returns value" $
    runErrorTranslateTest (tigerTest "test21.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "missing field in record" $
    runErrorTranslateTest (tigerTest "test22.tig") (`shouldSatisfy` isTypeCheckErrorAndMissingRecordField)

  it "type mismatch" $
    runErrorTranslateTest (tigerTest "test23.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "not array variable" $
    runErrorTranslateTest (tigerTest "test24.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedArrayType)

  it "not record variable" $
    runErrorTranslateTest (tigerTest "test25.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedRecordType)

  it "integer required" $
    runErrorTranslateTest (tigerTest "test26.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "different record type" $
    runErrorTranslateTest (tigerTest "test28.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "different array type" $
    runErrorTranslateTest (tigerTest "test29.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "init type differs from declared" $
    runErrorTranslateTest (tigerTest "test31.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "init type of array differed from declared" $
    runErrorTranslateTest (tigerTest "test32.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "unknown type" $
    runErrorTranslateTest (tigerTest "test33.tig") (`shouldSatisfy` isTypeCheckErrorAndUnknownType)

  it "type mismatched in function call" $
    runErrorTranslateTest (tigerTest "test34.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "less argument" $
    runErrorTranslateTest (tigerTest "test35.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "more argument" $
    runErrorTranslateTest (tigerTest "test36.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "type already declared" $
    runErrorTranslateTest (tigerTest "test38.tig") (`shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName)

  it "function already declared" $
    runErrorTranslateTest (tigerTest "test39.tig") (`shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName)

  it "procedure returns value" $
    runErrorTranslateTest (tigerTest "test40.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "type mismatch in addition" $
    runErrorTranslateTest (tigerTest "test43.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "mismatch initialization by nil" $
    runErrorTranslateTest (tigerTest "test45.tig") (`shouldSatisfy` isTypeCheckErrorAndNotDeterminedNilType)

complexTestSpec :: Spec
complexTestSpec = describe "complex integration test for tiger to translate" $ do
  it "merge.tig" $ do
    let merge = tigerTest "merge.tig"
    res <- translateTest merge
    res `shouldSatisfy` isRight

  it "queens.tig" $ do
    let merge = tigerTest "queens.tig"
    res <- translateTest merge
    res `shouldSatisfy` isRight

translateTest' :: FilePath -> IO (F.ProgramFragments FrameMock)
translateTest' = translateTest >=> either throwM pure

translateTest :: FilePath -> IO (Either SomeFrontendException (F.ProgramFragments FrameMock))
translateTest file = runIODef . runEitherEff @"frontendException" . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
  bs <- liftEff (Proxy @"IO") $ B.readFile file
  processFrontend @Tiger file bs

runErrorTranslateTest :: FilePath -> (SemantAnalysisError -> IO ()) -> IO ()
runErrorTranslateTest file assert = do
  (translateTest' file >> pure ())
    `frontendCatch` (\(ParserException msg) -> expectationFailure . T.unpack . textDisplay $ "parse error: " <> msg)
    `frontendCatch` (\(L _ e) -> assert e)
  where
    frontendCatch :: (MonadThrow m, MonadCatch m, FrontendException e) => m a -> (e -> m a) -> m a
    frontendCatch m f =
      m `catch` \e@(SomeFrontendException _) -> case fromFrontendException e of
        Nothing -> throwM e
        Just e -> f e
