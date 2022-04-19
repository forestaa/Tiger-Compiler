module Compiler.Frontend.Language.Tiger.LexerSpec (spec) where

import Compiler.Frontend.Language.Tiger.Lexer
import Compiler.Frontend.Lexer (ParserException)
import Control.Monad.Except
import Data.ByteString.Lazy qualified as B
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "lexer test" $
    it "samples/test**.tig" $ do
      let cases = ((:) '0' . show <$> [1 .. 9]) ++ (map show [10 .. 49])
          testcases = (++) <$> (("test/Compiler/Frontend/Language/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
      res <- runExceptT (traverse scannerTest testcases)
      res `shouldSatisfy` isRight

scannerTest :: FilePath -> ExceptT ParserException IO [Lexeme]
scannerTest file = do
  bs <- liftIO $ B.readFile file
  liftEither $ scanner file bs
