module Frontend.Language.Linear.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as B
import Frontend.Language.Linear.Parser
import Frontend.Lexer
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "parser test" $
    it "samples/test.ln" $ do
      let file = "test/Frontend/Language/Linear/samples/test.ln"
      bs <- B.readFile file
      runP parser file bs `shouldSatisfy` isRight
