module Linear.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as B
import Lexer.Monad
import Linear.Parser
import RIO
import Test.Hspec

spec :: Spec
spec =
  describe "parser test" $
    it "samples/test.ln" $ do
      let file = "test/Linear/samples/test.ln"
      bs <- B.readFile file
      runP parser file bs `shouldSatisfy` isRight
