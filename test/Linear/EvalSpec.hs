module Linear.EvalSpec (spec) where

import Data.ByteString.Lazy qualified as B
import Lexer.Monad
import Linear.Eval
import Linear.LSyntax
import Linear.Parser
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "parser test" $
    it "samples/test.ln" $ do
      let file = "test/Linear/samples/test.ln"
      bs <- B.readFile file
      case runP parser file bs of
        Left e -> expectationFailure $ show e
        Right ast -> runInit (unLStm ast) `shouldSatisfy` (\(r, o) -> isRight r && o == ["9", "8", "91", "8"])
