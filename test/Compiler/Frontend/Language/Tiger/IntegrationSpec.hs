module Compiler.Frontend.Language.Tiger.IntegrationSpec (spec) where

import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Exception (FrontendException (fromFrontendException), SomeFrontendException (SomeFrontendException))
import Compiler.Frontend.FrameMock
import Compiler.Frontend.Language.Tiger (Tiger (Tiger))
import Compiler.Frontend.Language.Tiger.Parser
import Compiler.Frontend.Language.Tiger.Semant
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level
import Compiler.Frontend.Language.Tiger.Semant.MarkEscape
import Compiler.Frontend.Language.Tiger.Semant.TypeCheck
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.TestUtils
import Compiler.Frontend.Lexer
import Compiler.Frontend.SrcLoc
import Compiler.Intermediate.Frame
import Compiler.Intermediate.Unique qualified as U
import Control.Exception.Safe (Exception (toException))
import Control.Monad (when)
import Control.Monad.Except
import Data.ByteString.Lazy qualified as B
import Data.Data (Proxy (Proxy))
import Data.Extensible
import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import Data.Maybe (isJust)
import GHC.Base (undefined)
import RIO
import Test.Hspec

spec :: Spec
spec = integrationSpec

integrationSpec :: Spec
integrationSpec = describe "integration test for translate" $ do
  it "then and else type differ" $
    runErrorTranslateTest (testcase "test09.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "body of while isTypeCheckErrorAnd not unit" $
    runErrorTranslateTest (testcase "test10.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedUnitType)

  it "hi in for is not int" $
    runErrorTranslateTest (testcase "test11.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "incompatible comparison: lt" $
    runErrorTranslateTest (testcase "test13.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "incompatible comparison: eq" $
    runErrorTranslateTest (testcase "test14.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "if-then returns non unit" $
    runErrorTranslateTest (testcase "test15.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedUnitType)

  it "invalid recursive type" $
    runErrorTranslateTest (testcase "test16.tig") (`shouldSatisfy` isTypeCheckErrorAndInvalidRecTypeDeclaration)

  it "invalid recursive type: interrupted" $
    runErrorTranslateTest (testcase "test17.tig") (`shouldSatisfy` isTypeCheckErrorAndUnknownType)

  it "invalid recursive function: interrupted" $
    runErrorTranslateTest (testcase "test18.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (testcase "test19.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (testcase "test20.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "procedure returns value" $
    runErrorTranslateTest (testcase "test21.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "missing field in record" $
    runErrorTranslateTest (testcase "test22.tig") (`shouldSatisfy` isTypeCheckErrorAndMissingRecordField)

  it "type mismatch" $
    runErrorTranslateTest (testcase "test23.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "not array variable" $
    runErrorTranslateTest (testcase "test24.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedArrayType)

  it "not record variable" $
    runErrorTranslateTest (testcase "test25.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedRecordType)

  it "integer required" $
    runErrorTranslateTest (testcase "test26.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "different record type" $
    runErrorTranslateTest (testcase "test28.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "different array type" $
    runErrorTranslateTest (testcase "test29.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "init type differs from declared" $
    runErrorTranslateTest (testcase "test31.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "init type of array differed from declared" $
    runErrorTranslateTest (testcase "test32.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "unknown type" $
    runErrorTranslateTest (testcase "test33.tig") (`shouldSatisfy` isTypeCheckErrorAndUnknownType)

  it "type mismatched in function call" $
    runErrorTranslateTest (testcase "test34.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "less argument" $
    runErrorTranslateTest (testcase "test35.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "more argument" $
    runErrorTranslateTest (testcase "test36.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "type already declared" $
    runErrorTranslateTest (testcase "test38.tig") (`shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName)

  it "function already declared" $
    runErrorTranslateTest (testcase "test39.tig") (`shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName)

  it "procedure returns value" $
    runErrorTranslateTest (testcase "test40.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "type mismatch in addition" $
    runErrorTranslateTest (testcase "test43.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "mismatch initialization by nil" $
    runErrorTranslateTest (testcase "test45.tig") (`shouldSatisfy` isTypeCheckErrorAndNotDeterminedNilType)

  it "valid test cases" $ do
    let testcases = (++) <$> (("test/Compiler/Frontend/Language/Tiger/samples/test" ++) <$> validTestCases) <*> [".tig"]
    res <- runExceptT (traverse (ExceptT . translateTest) testcases)
    res `shouldSatisfy` isRight

  it "merge.tig" $ do
    let merge = "test/Compiler/Frontend/Language/Tiger/samples/merge.tig"
    res <- translateTest merge
    res `shouldSatisfy` isRight

  it "queens.tig" $ do
    let merge = "test/Compiler/Frontend/Language/Tiger/samples/queens.tig"
    res <- translateTest merge
    res `shouldSatisfy` isRight
  where
    testcase s = "test/Compiler/Frontend/Language/Tiger/samples/" ++ s
    validTestCases = (\(d :: Integer) -> if d < 10 then '0' : show d else show d) <$> concat [[1 .. 8], [12], [27], [30], [37], [41 .. 42], [44], [46 .. 48]]

translateTest :: FilePath -> IO (Either SomeFrontendException [ProgramFragment FrameMock])
translateTest file = runIODef . runEitherEff @"exception" . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
  bs <- liftEff (Proxy :: Proxy "IO") $ B.readFile file
  processFrontend @Tiger file bs

runErrorTranslateTest :: FilePath -> (SemantAnalysisError -> IO ()) -> IO ()
runErrorTranslateTest file assert = do
  result <- translateTest file
  (either (throwM . toException) (const (pure ())) result) `catch` frontendExceptionHandler
  where
    frontendExceptionHandler :: SomeFrontendException -> IO ()
    frontendExceptionHandler e = do
      e `catch` (\(ParserException msg) -> expectationFailure $ "parse error: " ++ msg)
      e `catch` (\(L _ e :: RealLocated SemantAnalysisError) -> assert e)
      where
        catch e f = case fromFrontendException e of
          Just e -> f e
          Nothing -> pure ()
