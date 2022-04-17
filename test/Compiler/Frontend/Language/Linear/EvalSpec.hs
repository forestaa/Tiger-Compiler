module Compiler.Frontend.Language.Linear.EvalSpec (spec) where

import Compiler.Frontend.Language.Linear.Eval
import Compiler.Frontend.Language.Linear.LSyntax
import Compiler.Frontend.Language.Linear.Parser
import Compiler.Frontend.Lexer
import Data.ByteString.Lazy qualified as B
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "parser test" $
    it "samples/test.ln" $ do
      let file = "test/Compiler/Frontend/Language/Linear/samples/test.ln"
      bs <- B.readFile file
      case runP parser file bs of
        Left e -> expectationFailure $ show e
        Right ast -> runInit (unLStm ast) `shouldSatisfy` (\(r, o) -> isRight r && o == ["9", "8", "91", "8"])
