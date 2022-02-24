module Tiger.LexerSpec (spec) where

import Control.Monad.Except
import Data.ByteString.Lazy qualified as B
import RIO
import Test.Hspec
import Tiger.Lexer

spec :: Spec
spec =
  describe "lexer test" $
    it "samples/test**.tig" $ do
      let cases = ((:) '0' . show <$> [1 .. 9]) ++ (map show [10 .. 49])
          testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
      res <- runExceptT (traverse scannerTest testcases)
      res `shouldSatisfy` isRight

scannerTest :: FilePath -> ExceptT String IO [Lexeme]
scannerTest file = do
  bs <- liftIO $ B.readFile file
  liftEither $ scanner file bs
