module Compiler.Frontend.Language.Linear.LexerSpec (spec) where

import Compiler.Frontend.Language.Linear.Lexer
import Data.ByteString.Lazy qualified as B
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "lexer test" $
    it "samples/test.ln" $ do
      let file = "test/Compiler/Frontend/Language/Linear/samples/test.ln"
      bs <- B.readFile file
      scanner file bs `shouldSatisfy` isRight
