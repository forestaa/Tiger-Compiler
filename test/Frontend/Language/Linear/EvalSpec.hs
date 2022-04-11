module Frontend.Language.Linear.EvalSpec (spec) where

import Data.ByteString.Lazy qualified as B
import Frontend.Language.Linear.Eval
import Frontend.Language.Linear.LSyntax
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
      case runP parser file bs of
        Left e -> expectationFailure $ show e
        Right ast -> runInit (unLStm ast) `shouldSatisfy` (\(r, o) -> isRight r && o == ["9", "8", "91", "8"])
