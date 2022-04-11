module Frontend.Language.Linear.LexerSpec (spec) where

import Data.ByteString.Lazy qualified as B
import Frontend.Language.Linear.Lexer
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "lexer test" $
    it "samples/test.ln" $ do
      let file = "test/Frontend/Language/Linear/samples/test.ln"
      bs <- B.readFile file
      scanner file bs `shouldSatisfy` isRight
