module Compiler.Frontend.Language.Tiger.Semant.LevelSpec (spec) where

import Compiler.Frontend.FrameMock
import Compiler.Frontend.Language.Tiger.Semant.Level
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique
import Data.Extensible
import Data.Extensible.Effect
import RIO hiding (exp)
import Test.Hspec

spec :: Spec
spec = do
  pullInStaticLinksEffSpec
  fetchCurrentLevelParametersAccessEffSpec

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

fetchCurrentLevelParametersAccessEffSpec :: Spec
fetchCurrentLevelParametersAccessEffSpec = describe "fetch current level parameters access test" $ do
  it "non escape parameter" $ do
    let access = runEff $ do
          label <- newLabel
          withNewLevelEff label [False] $ do
            fetchCurrentLevelParametersAccessEff
    access `shouldSatisfy` \case
      [InReg _] -> True
      _ -> False

  it "escaped parameter" $ do
    let access = runEff $ do
          label <- newLabel
          withNewLevelEff label [True] $ do
            fetchCurrentLevelParametersAccessEff
    access `shouldSatisfy` \case
      [InFrame (-4)] -> True
      _ -> False

runEff :: Eff '["temp" >: UniqueEff, "label" >: UniqueEff, "nestingLevel" >: NestingLevelEff FrameMock] a -> a
runEff = fst . leaveEff . runNestingLevelEff . evalUniqueEff @"label" . evalUniqueEff @"temp"
