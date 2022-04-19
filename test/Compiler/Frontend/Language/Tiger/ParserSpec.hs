module Compiler.Frontend.Language.Tiger.ParserSpec (spec) where

import Compiler.Frontend.Language.Tiger.LSyntax
import Compiler.Frontend.Language.Tiger.Parser
import Compiler.Frontend.Lexer
import Compiler.Frontend.Lexer (ParserException)
import Control.Monad.Except
import Data.ByteString.Lazy qualified as B
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "parser test" $ do
    it "samples/test**.tig" $ do
      let cases = ((:) '0' . show <$> [1 .. 9]) ++ (map show [10 .. 48])
          testcases = (++) <$> (("test/Compiler/Frontend/Language/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
      res <- runExceptT (traverse parserTest testcases)
      res `shouldSatisfy` isRight
    it "samples/test49.tig" $ do
      let testcase = "test/Compiler/Frontend/Language/Tiger/samples/test49.tig"
      res <- runExceptT (parserTest testcase)
      res `shouldSatisfy` isLeft

parserTest :: FilePath -> ExceptT ParserException IO LExp
parserTest file = do
  bs <- liftIO $ B.readFile file
  liftEither $ runP parser file bs
