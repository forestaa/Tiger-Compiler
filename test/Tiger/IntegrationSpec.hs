module Tiger.IntegrationSpec (spec) where

import RIO
import Test.Hspec

import Tiger.Parser
import Tiger.Semant
import Tiger.Semant.Translate
import Tiger.Semant.Types
import Frame
import FrameMock

import Lexer.Monad

import Control.Monad.Except
import Data.Extensible
import qualified Data.ByteString.Lazy as B

spec :: Spec
spec = pure ()

integrationSpec :: Spec
integrationSpec =
  describe "integration test for translate" $ do
    it "then and else type differ" . runErrorTranslateTest $ testcase "test09.tig"
    it "body of while is not unit" . runErrorTranslateTest $ testcase "test10.tig"
    it "hi in for is not int" . runErrorTranslateTest $ testcase "test11.tig"
    it "incompatible comparison: lt" . runErrorTranslateTest $ testcase "test13.tig"
    it "incompatible comparison: eq" . runErrorTranslateTest $ testcase "test14.tig"
    it "if-then returns non unit" . runErrorTranslateTest $ testcase "test15.tig"
    it "invalid recursion type " . runErrorTranslateTest $ testcase "test16.tig"
    it "undefined variable" . runErrorTranslateTest $ testcase "test19.tig"
    it "undefined variable" . runErrorTranslateTest $ testcase "test20.tig"
    it "procedure returns value" . runErrorTranslateTest $ testcase "test21.tig"
    it "missing field in record" . runErrorTranslateTest $ testcase "test22.tig"
    it "type mismatch" . runErrorTranslateTest $ testcase "test23.tig"
    it "not array variable" . runErrorTranslateTest $ testcase "test24.tig"
    it "not record variable" . runErrorTranslateTest $ testcase "test25.tig"
    it "integer required" . runErrorTranslateTest $ testcase "test26.tig"
    it "different record type" . runErrorTranslateTest $ testcase "test28.tig"
    it "different array type" . runErrorTranslateTest $ testcase "test29.tig"
    it "init type differs from declared" . runErrorTranslateTest $ testcase "test31.tig"
    it "init type of array differed from declared" . runErrorTranslateTest $ testcase "test32.tig"
    it "unknown type" . runErrorTranslateTest $ testcase "test33.tig"
    it "type mismatched in function call" . runErrorTranslateTest $ testcase "test34.tig"
    it "less argument" . runErrorTranslateTest $ testcase "test35.tig"
    it "more argument" . runErrorTranslateTest $ testcase "test36.tig"
    it "type already declared" . runErrorTranslateTest $ testcase "test38.tig"
    it "function already declared" . runErrorTranslateTest $ testcase "test39.tig"
    it "procedure returns value" . runErrorTranslateTest $ testcase "test40.tig"
    it "type mismatch in addition" . runErrorTranslateTest $ testcase "test43.tig"
    it "mismatch initialization by nil" . runErrorTranslateTest $ testcase "test45.tig"
    it "type already declared" . runErrorTranslateTest $ testcase "test47.tig"
    it "function already declared" . runErrorTranslateTest $ testcase "test48.tig"
    it "valid test" $ do
      let testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> valid) <*> [".tig"]
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
    valid = (\(d :: Integer) -> if d < 10 then '0' : show d else show d) <$> concat [[1..8], [12], [17..18], [27], [30], [37], [41..42], [44], [46]]

translateTest :: FilePath -> ExceptT String IO ((Exp, Type), [ProgramFragment FrameMock])
translateTest file = do
  bs <- liftIO $ B.readFile file
  e <- liftEither $ runP parser file bs
  liftEither . mapLeft show . leaveEff . runTranslateEffWithNewLevel $ translateExp e

runErrorTranslateTest :: FilePath -> Expectation
runErrorTranslateTest file = do
  res <- runExceptT (translateTest file)
  res `shouldSatisfy` isLeft
