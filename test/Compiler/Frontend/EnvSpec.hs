module Compiler.Frontend.EnvSpec (spec) where

import Compiler.Frontend.Env qualified as Env
import Data.Extensible.Effect (evalStateEff, getsEff, leaveEff, modifyEff)
import RIO
import Test.Hspec

spec :: Spec
spec = describe "Env test" $ do
  it "value which is not inserted can not be looked up" $ do
    Env.lookup "x" (Env.insert "x" 1 Env.empty) `shouldBe` Just 1

  it "value which is inserted can be looked up" $ do
    Env.lookup "x" (Env.insert "x" 1 Env.empty) `shouldBe` Just 1

  it "adjusted value can be looked up" $ do
    Env.lookup "x" (Env.adjust (+ 1) "x" (Env.insert "x" 1 Env.empty)) `shouldBe` Just 2

  it "withEnvScope creates scope" $ do
    let (a, b, c) = leaveEff . flip (evalStateEff @"env") Env.empty $ do
          (a, b) <- Env.withEnvScope #env $ do
            modifyEff #env $ Env.insert "x" 1
            a <- Env.withEnvScope #env $ do
              modifyEff #env $ Env.insert "x" 2
              getsEff #env $ Env.lookup "x"
            b <- getsEff #env $ Env.lookup "x"
            pure (a, b)
          c <- getsEff #env $ Env.lookup "x"
          pure (a, b, c)
    (a, b, c) `shouldBe` (Just 2, Just 1, Nothing)
