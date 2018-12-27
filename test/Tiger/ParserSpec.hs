module Tiger.ParserSpec (spec) where

import Test.Hspec
import Tiger.Parser
import Tiger.LSyntax

import Lexer.Monad

import RIO
import qualified Data.ByteString.Lazy as B

import Control.Monad.Except

spec :: Spec
spec =
  describe "parser test" $ do
    it "samples/test**.tig" $ do
      let cases = ((:) '0' . show <$> [1..9]) ++ (map show [10..48])
          testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
      res <- runExceptT (traverse parserTest testcases)
      res `shouldSatisfy` isRight
    it "samples/test49.tig" $ do
      let testcase = "test/Tiger/samples/test49.tig"
      res <- runExceptT (parserTest testcase)
      res `shouldSatisfy` isLeft

parserTest :: FilePath -> ExceptT String IO LExp
parserTest file = do
  bs <- liftIO $ B.readFile file
  liftEither $ runP parser file bs
