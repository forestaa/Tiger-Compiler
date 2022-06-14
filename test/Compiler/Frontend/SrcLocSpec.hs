module Compiler.Frontend.SrcLocSpec (spec) where

import Compiler.Frontend.Exception (FrontendException (fromFrontendException, toFrontendException), SomeFrontendException (..), frontendExceptionFromException, frontendExceptionToException)
import Compiler.Frontend.SrcLoc
import Control.Exception.Safe (MonadCatch, catch)
import Data.Data (cast)
import RIO hiding (catch)
import Test.Hspec

spec :: Spec
spec = realLocatedExceptionSpec

realLocatedExceptionSpec :: Spec
realLocatedExceptionSpec = describe "RealLocatedExceptionSpec" $ do
  it "hierarchy 0" $ do
    let le = dummyRealLocated TestFrontendException0
    frontendThrow le
      `realLocatedCatch` \e@TestFrontendException0 -> e `shouldBe` TestFrontendException0
  it "hierarchy 1" $ do
    let le = dummyRealLocated TestFrontendException1
    frontendThrow le
      `realLocatedCatch` (\e@TestFrontendException1 -> e `shouldBe` TestFrontendException1)
  it "hierarchy 2" $ do
    let le = dummyRealLocated TestFrontendException2
    frontendThrow le
      `realLocatedCatch` (\e@TestFrontendException2 -> e `shouldBe` TestFrontendException2)
  it "hierarchy 2" $ do
    let le = dummyRealLocated TestFrontendException3
    frontendThrow le
      `realLocatedCatch` (\e@TestFrontendException3 -> e `shouldBe` TestFrontendException3)

frontendThrow :: (MonadThrow m, FrontendException e) => e -> m a
frontendThrow = throwM . toFrontendException

realLocatedCatch :: (MonadThrow m, MonadCatch m, FrontendException e) => m () -> (e -> m ()) -> m ()
realLocatedCatch m f =
  m `catch` \e@(SomeFrontendException _) ->
    case fromFrontendException e of
      Nothing -> frontendThrow e
      Just (L _ e') -> f e'

data TestFrontendException0 = TestFrontendException0 deriving (Show, Eq)

instance FrontendException TestFrontendException0 where
  toFrontendException = frontendExceptionToException
  fromFrontendException = frontendExceptionFromException

data SomeTestFrontendException1 = forall e. FrontendException e => SomeTestFrontendException1 e

instance Show SomeTestFrontendException1 where
  show (SomeTestFrontendException1 e) = show e

instance FrontendException SomeTestFrontendException1 where
  toFrontendException = frontendExceptionToException
  fromFrontendException = frontendExceptionFromException

data TestFrontendException1 = TestFrontendException1 deriving (Show, Eq)

instance FrontendException TestFrontendException1 where
  toFrontendException = toFrontendException . SomeTestFrontendException1
  fromFrontendException e = do
    SomeTestFrontendException1 e <- fromFrontendException e
    cast e

data SomeTestFrontendException2 = forall e. FrontendException e => SomeTestFrontendException2 e

instance Show SomeTestFrontendException2 where
  show (SomeTestFrontendException2 e) = show e

instance FrontendException SomeTestFrontendException2 where
  toFrontendException = toFrontendException . SomeTestFrontendException1
  fromFrontendException x = do
    SomeTestFrontendException1 x <- fromFrontendException x
    cast x

data TestFrontendException2 = TestFrontendException2 deriving (Show, Eq)

instance FrontendException TestFrontendException2 where
  toFrontendException = toFrontendException . SomeTestFrontendException2
  fromFrontendException e = do
    SomeTestFrontendException2 e <- fromFrontendException e
    cast e

data SomeTestFrontendException3 = forall e. FrontendException e => SomeTestFrontendException3 e

instance Show SomeTestFrontendException3 where
  show (SomeTestFrontendException3 e) = show e

instance FrontendException SomeTestFrontendException3 where
  toFrontendException = toFrontendException . SomeTestFrontendException2
  fromFrontendException x = do
    SomeTestFrontendException2 x <- fromFrontendException x
    cast x

data TestFrontendException3 = TestFrontendException3 deriving (Show, Eq)

instance FrontendException TestFrontendException3 where
  toFrontendException = toFrontendException . SomeTestFrontendException3
  fromFrontendException e = do
    SomeTestFrontendException3 e <- fromFrontendException e
    cast e
