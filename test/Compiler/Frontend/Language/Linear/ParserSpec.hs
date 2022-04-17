module Compiler.Frontend.Language.Linear.ParserSpec (spec) where

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
      runP parser file bs `shouldSatisfy` isRight
