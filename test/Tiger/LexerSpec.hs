module Tiger.LexerSpec (spec) where

import Test.Hspec
import Tiger.Lexer

import RIO
import qualified Data.ByteString.Lazy as B

import Control.Monad.Except

spec :: Spec
spec = do
  describe "lexer test" $ do
    it "samples/test**.tig" $ do
      let cases = ((:) '0' . show <$> [1..9]) ++ (map show [10..49])
          testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
      res <- runExceptT (traverse scannerTest testcases)
      res `shouldSatisfy` isRight

scannerTest :: FilePath -> ExceptT String IO [Lexeme]
scannerTest file = do
  bs <- liftIO $ B.readFile file
  liftEither $ scanner file bs
