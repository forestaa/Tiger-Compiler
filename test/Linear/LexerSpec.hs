module Linear.LexerSpec (spec) where

import Test.Hspec
import Linear.Lexer

import RIO
import qualified Data.ByteString.Lazy as B

spec :: Spec
spec = do
  describe "lexer test" $ do
    it "samples/test.ln" $ do
      let file = "test/Linear/samples/test.ln"
      bs <- B.readFile file
      scanner file bs `shouldSatisfy` isRight
