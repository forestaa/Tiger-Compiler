module Compiler.Utils.MaybeSpec (spec) where

import Compiler.Utils.Maybe
import RIO
import Test.Hspec

data Hoge = Hoge {int :: Int} deriving (Eq, Show)

data Fuga = Fuga {hoge :: Maybe Hoge} deriving (Eq, Show)

data Pohe = Pohe {fuga :: Maybe Fuga} deriving (Eq, Show)

spec :: Spec
spec = describe "optional chaining" $ do
  it "Maybe can be accessed by dot" $ do
    let nothing = Nothing :: Maybe Hoge
    nothing.int `shouldBe` Nothing
    let justHoge = Just (Hoge 2)
    justHoge.int `shouldBe` Just 2

  it "optinahining" $ do
    let nothing = Nothing :: Maybe Pohe
    (nothing ?. (Proxy :: Proxy "fuga") ?. (Proxy :: Proxy "hoge")).int `shouldBe` Nothing

    let justPohe = Just (Pohe Nothing)
    (justPohe ?. (Proxy :: Proxy "fuga") ?. (Proxy :: Proxy "hoge")).int `shouldBe` Nothing

    let justFuga = Just (Pohe (Just (Fuga Nothing)))
    (justFuga ?. (Proxy :: Proxy "fuga") ?. (Proxy :: Proxy "hoge")).int `shouldBe` Nothing

    let justHoge = Just (Pohe (Just (Fuga (Just (Hoge 2)))))
    (justHoge ?. (Proxy :: Proxy "fuga") ?. (Proxy :: Proxy "hoge")).int `shouldBe` Just 2
