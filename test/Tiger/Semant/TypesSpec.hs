module Tiger.Semant.TypesSpec (spec, runUniqueLevelEff) where

import Test.Hspec
import Tiger.Semant.Types
import qualified Frame as F
import qualified IR
import qualified Unique as U
import FrameMock

import RIO hiding (exp)
import Data.Extensible


spec :: Spec
spec = getLevelExpSpec

getLevelExpSpec :: Spec
getLevelExpSpec = describe "getLevelExp test" $ do
  it "depth 1" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    level1 <- newLevel @FrameMock label1 []
    me <- pullInStaticLinksEff level1
    pure $ me `shouldBe` Just (IR.Temp (F.fp @FrameMock))
  it "depth 2" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    level1 <- newLevel @FrameMock label1 []
    label2 <- U.newLabel
    _ <- newLevel label2 []
    me <- pullInStaticLinksEff level1
    pure $ me `shouldBe` Just (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp (F.fp @FrameMock))))
  it "with arguments" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    label2 <- U.newLabel
    label3 <- U.newLabel
    level1 <- newLevel @FrameMock label1 [True, False]
    _ <- newLevel label2 [True, True]
    _ <- newLevel label3 [False]
    me <- pullInStaticLinksEff level1
    pure $ me `shouldBe` Just (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp (F.fp @FrameMock))))))
  it "with local variables" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    label2 <- U.newLabel
    level1 <- newLevel @FrameMock label1 []
    _ <- allocateLocalOnCurrentLevel True
    _ <- newLevel label2 []
    me <- pullInStaticLinksEff level1
    pure $ me `shouldBe` Just (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp (F.fp @FrameMock))))


runUniqueLevelEff :: Eff '["temp" >: U.UniqueEff, "label" >: U.UniqueEff, "nestingLevel" >: NestingLevelEff f] a -> a
runUniqueLevelEff = leaveEff . runNestingLevelEff . U.runUniqueEff @"label" . U.runUniqueEff @"temp"
