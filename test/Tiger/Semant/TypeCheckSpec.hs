module Tiger.Semant.TypeCheckSpec (spec) where

import           Control.Monad.Trans.Cont
import           Data.Extensible
import           Data.Extensible.Effect
import           RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as Partial
import qualified RIO.Map as Map
import           Test.Hspec

import qualified Frame as F
import           FrameMock
import qualified IR
import           SrcLoc
import           TestUtils
import           Unique

import           Tiger.Semant
import           Tiger.Semant.BreakPoint
import           Tiger.Semant.Env
import           Tiger.Semant.Exp
import           Tiger.Semant.Level
import           Tiger.Semant.Translate
import           Tiger.Semant.TypeCheck
import           Tiger.Semant.Types
import qualified Tiger.LSyntax as T (valueToLValue, expToLExp)
import qualified Tiger.Syntax as T


spec :: Spec
spec = typeCheckArrayIndexSpec

typeCheckArrayIndexSpec :: Spec
typeCheckArrayIndexSpec = describe "type check array index test" $ do
  it "var a: array int; a[0]" $ do
    let result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
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
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
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



runEff :: Eff '[
        ("typeEnv" >: State TEnv)
      , ("id" >: UniqueEff)
      , ("typeCheckError" >: EitherEff (RealLocated TypeCheckError))
      ] a
  -> Either (RealLocated TypeCheckError) a
runEff = leaveEff . runEitherEff @"typeCheckError" . runUniqueEff @"id" . evalTEnvEff initTEnv
