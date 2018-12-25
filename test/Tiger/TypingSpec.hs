module Tiger.TypingSpec (spec) where

import Test.Hspec
import Tiger.Parser
import Tiger.LSyntax
import Tiger.Typing

import Lexer.Monad

import RIO
import qualified Data.ByteString.Lazy as B

import Control.Monad.Except
import SrcLoc

spec :: Spec
spec = do
  describe "parser test" $ do
    it "invalid_rectype.tig" $ do
      let testcase = "test/Tiger/samples/invalid_rectype.tig"
      res <- runExceptT (typingTest testcase)
      res `shouldSatisfy` isLeft
    it "all test" $ do
      let cases = ((:) '0' . show <$> [1..9]) ++ (map show [10..48])
          testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
      res <- runExceptT (traverse typingTest testcases)
      res `shouldSatisfy` isRight

typingTest :: FilePath -> ExceptT String IO Type
typingTest file = do
  bs <- liftIO $ B.readFile file
  e <- liftEither $ runP parser file bs
  liftEither . mapLeft show . runTyping $ typingExp e