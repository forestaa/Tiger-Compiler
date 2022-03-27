module Tiger.Semant.TypeCheckSpec (spec) where

import Control.Monad.Trans.Cont
import Data.Extensible
import Data.Extensible.Effect
import Frame qualified as F
import FrameMock
import IR qualified
import RIO
import RIO.List qualified as List
import RIO.List.Partial qualified as Partial
import RIO.Map qualified as Map
import SrcLoc
import Test.Hspec
import TestUtils
import Tiger.LSyntax qualified as T (expToLExp, valueToLValue)
import Tiger.Semant
import Tiger.Semant.BreakPoint
import Tiger.Semant.Env
import Tiger.Semant.Exp
import Tiger.Semant.Level
import Tiger.Semant.Translate
import Tiger.Semant.TypeCheck
import Tiger.Semant.Types
import Tiger.Syntax qualified as T
import Unique

spec :: Spec
spec = typeCheckArrayIndexSpec

typeCheckArrayIndexSpec :: Spec
typeCheckArrayIndexSpec = describe "type check array index test" $ do
  it "var a: array int; a[0]" $ do
    let result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "a" arrayTy
          (_, cont) <- typeCheckArrayIndex (dummyRealLocated (T.valueToLValue $ T.Id "a", T.expToLExp $ T.Int 0))
          (_, cont) <- cont arrayTy
          cont TInt
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ty -> ty `shouldBe` TInt

  it "var a: array int; a['hoge']" $ do
    let result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray TInt id
          insertType "a" arrayTy
          (_, cont) <- typeCheckArrayIndex (dummyRealLocated (T.valueToLValue $ T.Id "a", T.expToLExp $ T.String "hoge"))
          (_, cont) <- cont arrayTy
          cont TString
    case result of
      Right ty -> expectationFailure $ "should return ExpectedIntType, but got " ++ show ty
      Left (L _ e) -> e `shouldSatisfy` isExpectedIntType

  it "var x: int; a[0]" $ do
    let result = runEff $ do
          insertType "a" TInt
          (_, cont) <- typeCheckArrayIndex (dummyRealLocated (T.valueToLValue $ T.Id "a", T.expToLExp $ T.Int 0))
          (_, cont) <- cont TInt
          cont TInt
    case result of
      Right ty -> expectationFailure $ "should return ExpectedArrayType, but got " ++ show ty
      Left (L _ e) -> e `shouldSatisfy` isExpectedArrayType

runEff ::
  Eff
    '[ ("typeEnv" >: State TEnv),
       ("id" >: UniqueEff),
       ("typeCheckError" >: EitherEff (RealLocated TypeCheckError))
     ]
    a ->
  Either (RealLocated TypeCheckError) a
runEff = leaveEff . runEitherEff @"typeCheckError" . evalUniqueEff @"id" . evalTEnvEff initTEnv
