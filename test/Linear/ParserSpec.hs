module Linear.ParserSpec (spec) where

import Test.Hspec
import Linear.Parser
import Lexer.Monad

import RIO
import qualified Data.ByteString.Lazy as B

spec :: Spec
spec = do
  describe "parser test" $ do
    it "samples/test.ln" $ do
      let file = "test/Linear/samples/test.ln"
      bs <- B.readFile file
      runP parser file bs `shouldSatisfy` isRight
