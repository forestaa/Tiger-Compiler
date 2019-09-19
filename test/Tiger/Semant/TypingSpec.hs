module Tiger.Semant.TypingSpec (spec) where

import Test.Hspec
import Tiger.Parser
import Tiger.Semant.Typing

import Lexer.Monad

import RIO
import qualified Data.ByteString.Lazy as B

import Control.Monad.Except

spec :: Spec
spec =
  describe "typing test" $ do
    it "then and else type differ" . runErrorTypingTest $ testcase "test09.tig"
    it "body of while is not unit" . runErrorTypingTest $ testcase "test10.tig"
    it "hi in for is not int" . runErrorTypingTest $ testcase "test11.tig"
    it "incompatible comparison: lt" . runErrorTypingTest $ testcase "test13.tig"
    it "incompatible comparison: eq" . runErrorTypingTest $ testcase "test14.tig"
    it "if-then returns non unit" . runErrorTypingTest $ testcase "test15.tig"
    it "invalid recursion type " . runErrorTypingTest $ testcase "test16.tig"
    it "undefined variable" . runErrorTypingTest $ testcase "test19.tig"
    it "undefined variable" . runErrorTypingTest $ testcase "test20.tig"
    it "procedure returns value" . runErrorTypingTest $ testcase "test21.tig"
    it "missing field in record" . runErrorTypingTest $ testcase "test22.tig"
    it "type mismatch" . runErrorTypingTest $ testcase "test23.tig"
    it "not array variable" . runErrorTypingTest $ testcase "test24.tig"
    it "not record variable" . runErrorTypingTest $ testcase "test25.tig"
    it "integer required" . runErrorTypingTest $ testcase "test26.tig"
    it "different record type" . runErrorTypingTest $ testcase "test28.tig"
    it "different array type" . runErrorTypingTest $ testcase "test29.tig"
    it "init type differs from declared" . runErrorTypingTest $ testcase "test31.tig"
    it "init type of array differed from declared" . runErrorTypingTest $ testcase "test32.tig"
    it "unknown type" . runErrorTypingTest $ testcase "test33.tig"
    it "type mismatched in function call" . runErrorTypingTest $ testcase "test34.tig"
    it "less argument" . runErrorTypingTest $ testcase "test35.tig"
    it "more argument" . runErrorTypingTest $ testcase "test36.tig"
    it "type already declared" . runErrorTypingTest $ testcase "test38.tig"
    it "function already declared" . runErrorTypingTest $ testcase "test39.tig"
    it "procedure returns value" . runErrorTypingTest $ testcase "test40.tig"
    it "type mismatch in addition" . runErrorTypingTest $ testcase "test43.tig"
    it "mismatch initialization by nil" . runErrorTypingTest $ testcase "test45.tig"
    it "type already declared" . runErrorTypingTest $ testcase "test47.tig"
    it "function already declared" . runErrorTypingTest $ testcase "test48.tig"
    it "valid test" $ do
      let testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> valid) <*> [".tig"]
      res <- runExceptT (traverse typingTest testcases)
      res `shouldSatisfy` isRight
    it "merge.tig" $ do
      let merge = "test/Tiger/samples/merge.tig"
      res <- runExceptT (typingTest merge)
      res `shouldSatisfy` isRight
    it "queens.tig" $ do
      let merge = "test/Tiger/samples/queens.tig"
      res <- runExceptT (typingTest merge)
      res `shouldSatisfy` isRight
  where
    testcase s = "test/Tiger/samples/" ++ s
    valid = (\(d :: Integer) -> if d < 10 then '0' : show d else show d) <$> concat [[1..8], [12], [17..18], [27], [30], [37], [41..42], [44], [46]]

typingTest :: FilePath -> ExceptT String IO Type
typingTest file = do
  bs <- liftIO $ B.readFile file
  e <- liftEither $ runP parser file bs
  liftEither . mapLeft show . runTyping $ typingExp e

runErrorTypingTest :: FilePath -> Expectation
runErrorTypingTest file = do
  res <- runExceptT (typingTest file)
  res `shouldSatisfy` isLeft
