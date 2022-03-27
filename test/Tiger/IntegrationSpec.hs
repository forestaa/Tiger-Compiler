module Tiger.IntegrationSpec (spec) where

import Control.Monad.Except
import Data.ByteString.Lazy qualified as B
import Data.Extensible
import Data.Extensible.Effect
import Frame
import FrameMock
import Lexer.Monad
import RIO
import SrcLoc
import Test.Hspec
import TestUtils
import Tiger.Parser
import Tiger.Semant
import Tiger.Semant.Exp
import Tiger.Semant.Level
import Tiger.Semant.MarkEscape
import Tiger.Semant.TypeCheck
import Tiger.Semant.Types

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
    let testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> validTestCases) <*> [".tig"]
    res <- runExceptT (traverse translateTest testcases)
    res `shouldSatisfy` isRight

  it "merge.tig" $ do
    let merge = "test/Tiger/samples/merge.tig"
    res <- runExceptT (translateTest merge)
    res `shouldSatisfy` isRight

  it "queens.tig" $ do
    let merge = "test/Tiger/samples/queens.tig"
    res <- runExceptT (translateTest merge)
    res `shouldSatisfy` isRight
  where
    testcase s = "test/Tiger/samples/" ++ s
    validTestCases = (\(d :: Integer) -> if d < 10 then '0' : show d else show d) <$> concat [[1 .. 8], [12], [27], [30], [37], [41 .. 42], [44], [46 .. 48]]

data Error = ParseError String | SemantAnalysisError SemantAnalysisError deriving (Show)

translateTest :: FilePath -> ExceptT Error IO (((Exp, Type), NestingLevel FrameMock), [ProgramFragment FrameMock])
translateTest file = do
  bs <- liftIO $ B.readFile file
  e <- liftEither . mapLeft ParseError $ runP parser file bs
  liftEither . mapLeft (SemantAnalysisError . unLoc) . leaveEff . evalTranslateEffWithNewLevel $ do
    insertInitVAEnv
    insertInitVTEnv
    translateExp $ markEscape e

runErrorTranslateTest :: FilePath -> (SemantAnalysisError -> IO ()) -> IO ()
runErrorTranslateTest file assert =
  runExceptT (translateTest file) >>= \case
    Right _ -> pure ()
    Left (ParseError msg) -> expectationFailure $ "parse error: " ++ msg
    Left (SemantAnalysisError e) -> assert e
