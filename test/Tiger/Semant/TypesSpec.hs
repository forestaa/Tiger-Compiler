module Tiger.Semant.TypesSpec (spec) where

import Test.Hspec
import Tiger.Semant.Types
import qualified Frame as F
import qualified IR
import Unique
import FrameMock

import RIO hiding (exp)
import Data.Extensible


spec :: Spec
spec = pullInStaticLinksEffSpec

pullInStaticLinksEffSpec :: Spec
pullInStaticLinksEffSpec = describe "pullInStaticLinksEff test" $ do
  it "1 depth" $ do
    let exp = runEff $ do
          label <- newLabel
          withNewLevelEff label [False] $ do
            level <- fetchCurrentLevelEff
            pullInStaticLinksEff level
    exp `shouldSatisfy` \case
      (IR.Temp fp) -> fp == F.fp @FrameMock
      _ -> False

  it "2 depth" $ do
    let exp = runEff $ do
          label <- newLabel
          withNewLevelEff label [False] $ do
            level <- fetchCurrentLevelEff
            withNewLevelEff label [True] $ do
              pullInStaticLinksEff level
    exp `shouldSatisfy` \case
      (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp))) -> fp == F.fp @FrameMock
      _ -> False

  it "3 depth" $ do
    let exp = runEff $ do
          label <- newLabel
          withNewLevelEff label [False] $ do
            level <- fetchCurrentLevelEff
            withNewLevelEff label [True] $ do
              withNewLevelEff label [True] $ do
                pullInStaticLinksEff level
    exp `shouldSatisfy` \case
      (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp))))) -> fp == F.fp @FrameMock
      _ -> False


runEff :: Eff '["temp" >: UniqueEff, "label" >: UniqueEff, "nestingLevel" >: NestingLevelEff FrameMock] a -> a
runEff = leaveEff . runNestingLevelEff . runUniqueEff @"label" . runUniqueEff @"temp"
