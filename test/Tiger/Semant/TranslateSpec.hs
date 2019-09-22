module Tiger.Semant.TranslateSpec where

import Test.Hspec
import Tiger.Semant.Translate
import Tiger.Semant.Types
import Tiger.Semant.TypesSpec
import qualified Frame as F
import qualified IR
import qualified Unique as U
import FrameMock

import RIO
import Data.Extensible

spec :: Spec
spec = valueIdExpSpec

valueIdExpSpec :: Spec
valueIdExpSpec = describe "simpleVarExp test" $
  it "current level" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    level1 <- newLevel label1 []
    allocateLocalOnCurrentLevel True >>= \case
      Nothing -> pure $ expectationFailure "something happens"
      Just access -> do
        me <- valueIdExp (Access (#level @= (level1 :: Level FrameMock) <: #access @= access <: nil))
        pure $ me `shouldBe` Just (Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (F.fp (Proxy :: Proxy FrameMock))))))
