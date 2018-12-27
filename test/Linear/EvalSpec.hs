module Linear.EvalSpec (spec) where

import Test.Hspec
import Linear.Eval
import Linear.LSyntax
import Linear.Parser
import Lexer.Monad

import RIO
import qualified Data.ByteString.Lazy as B

spec :: Spec
spec =
  describe "parser test" $
    it "samples/test.ln" $ do
      let file = "test/Linear/samples/test.ln"
      bs <- B.readFile file
      case runP parser file bs of
        Left e -> expectationFailure $ show e
        Right ast -> runInit (unLStm ast) `shouldSatisfy` (\(r, o) -> isRight r && o == ["9", "8", "91", "8"])
