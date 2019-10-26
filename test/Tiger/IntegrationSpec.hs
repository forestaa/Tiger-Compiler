module Tiger.IntegrationSpec (spec) where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import           Data.Extensible
import           RIO
import           Test.Hspec

import           Frame
import           FrameMock
import           Lexer.Monad
import           SrcLoc
import           TestUtils

import           Tiger.Parser
import           Tiger.Semant
import           Tiger.Semant.Exp
import           Tiger.Semant.MarkEscape
import           Tiger.Semant.TypeCheck
import           Tiger.Semant.Types



spec :: Spec
spec = integrationSpec

integrationSpec :: Spec
integrationSpec = describe "integration test for translate" $ do
  it "then and else type differ" $
    runErrorTranslateTest (testcase "test09.tig") (`shouldSatisfy` isExpectedType)

  it "body of while is not unit" $
    runErrorTranslateTest (testcase "test10.tig") (`shouldSatisfy` isExpectedUnitType)

  it "hi in for is not int" $
    runErrorTranslateTest (testcase "test11.tig") (`shouldSatisfy` isExpectedIntType)

  it "incompatible comparison: lt" $
    runErrorTranslateTest (testcase "test13.tig") (`shouldSatisfy` isExpectedIntType)

  it "incompatible comparison: eq" $
    runErrorTranslateTest (testcase "test14.tig") (`shouldSatisfy` isExpectedType)

  it "if-then returns non unit" $
    runErrorTranslateTest (testcase "test15.tig") (`shouldSatisfy` isExpectedUnitType)

  it "invalid recursive type" $
    runErrorTranslateTest (testcase "test16.tig") (`shouldSatisfy` isInvalidRecTypeDeclaration)

  it "invalid recursive type: interrupted" $
    runErrorTranslateTest (testcase "test17.tig") (`shouldSatisfy` isUnknownType)

  it "invalid recursive function: interrupted" $
    runErrorTranslateTest (testcase "test18.tig") (`shouldSatisfy` isUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (testcase "test19.tig") (`shouldSatisfy` isUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (testcase "test20.tig") (`shouldSatisfy` isUndefinedVariable)

  it "procedure returns value" $
    runErrorTranslateTest (testcase "test21.tig") (`shouldSatisfy` isExpectedIntType)

  it "missing field in record" $
    runErrorTranslateTest (testcase "test22.tig") (`shouldSatisfy` isMissingRecordField)

  it "type mismatch" $
    runErrorTranslateTest (testcase "test23.tig") (`shouldSatisfy` isExpectedType)

  it "not array variable" $
    runErrorTranslateTest (testcase "test24.tig") (`shouldSatisfy` isExpectedArrayType)

  it "not record variable" $
    runErrorTranslateTest (testcase "test25.tig") (`shouldSatisfy` isExpectedRecordType)

  it "integer required" $
    runErrorTranslateTest (testcase "test26.tig") (`shouldSatisfy` isExpectedIntType)

  it "different record type" $
    runErrorTranslateTest (testcase "test28.tig") (`shouldSatisfy` isExpectedType)

  it "different array type" $
    runErrorTranslateTest (testcase "test29.tig") (`shouldSatisfy` isExpectedType)

  it "init type differs from declared" $
    runErrorTranslateTest (testcase "test31.tig") (`shouldSatisfy` isExpectedType)

  it "init type of array differed from declared" $
    runErrorTranslateTest (testcase "test32.tig") (`shouldSatisfy` isExpectedType)

  it "unknown type" $
    runErrorTranslateTest (testcase "test33.tig") (`shouldSatisfy` isUnknownType)

  it "type mismatched in function call" $
    runErrorTranslateTest (testcase "test34.tig") (`shouldSatisfy` isExpectedTypes)

  it "less argument" $
    runErrorTranslateTest (testcase "test35.tig") (`shouldSatisfy` isExpectedTypes)

  it "more argument" $
    runErrorTranslateTest (testcase "test36.tig") (`shouldSatisfy` isExpectedTypes)

  it "type already declared" $
    runErrorTranslateTest (testcase "test38.tig") (`shouldSatisfy` isMultiDeclaredName)

  it "function already declared" $
    runErrorTranslateTest (testcase "test39.tig") (`shouldSatisfy` isMultiDeclaredName)

  it "procedure returns value" $
    runErrorTranslateTest (testcase "test40.tig") (`shouldSatisfy` isExpectedType)

  it "type mismatch in addition" $
    runErrorTranslateTest (testcase "test43.tig") (`shouldSatisfy` isExpectedIntType)

  it "mismatch initialization by nil" $
    runErrorTranslateTest (testcase "test45.tig") (`shouldSatisfy` isNotDeterminedNilType)

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
    validTestCases = (\(d :: Integer) -> if d < 10 then '0' : show d else show d) <$> concat [[1..8], [12], [27], [30], [37], [41..42], [44], [46..48]]


data Error = ParseError String | TranslateError TranslateError deriving Show
translateTest :: FilePath -> ExceptT Error IO ((Exp, Type), [ProgramFragment FrameMock])
translateTest file = do
  bs <- liftIO $ B.readFile file
  e <- liftEither . mapLeft ParseError $ runP parser file bs
  liftEither . mapLeft (TranslateError . unLoc) . leaveEff . runTranslateEffWithNewLevel $ do
    insertInitVEnv
    translateExp $ markEscape e

runErrorTranslateTest :: FilePath -> (TranslateError -> IO ()) -> IO ()
runErrorTranslateTest file assert =
  runExceptT (translateTest file) >>= \case
    Right _ -> pure ()
    Left (ParseError msg) -> expectationFailure $ "parse error: " ++ msg
    Left (TranslateError e) -> assert e
