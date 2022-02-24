module Linear.LexerSpec (spec) where

import Data.ByteString.Lazy qualified as B
import Linear.Lexer
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "lexer test" $
    it "samples/test.ln" $ do
      let file = "test/Linear/samples/test.ln"
      bs <- B.readFile file
      scanner file bs `shouldSatisfy` isRight
