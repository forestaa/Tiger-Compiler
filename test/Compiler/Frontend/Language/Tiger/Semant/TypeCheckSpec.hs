module Compiler.Frontend.Language.Tiger.Semant.TypeCheckSpec (spec) where

import Compiler.Frontend.FrameMock
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T (expToLExp, valueToLValue)
import Compiler.Frontend.Language.Tiger.Semant
import Compiler.Frontend.Language.Tiger.Semant.BreakPoint
import Compiler.Frontend.Language.Tiger.Semant.Env
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level
import Compiler.Frontend.Language.Tiger.Semant.Translate
import Compiler.Frontend.Language.Tiger.Semant.TypeCheck
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.Language.Tiger.TestUtils (isExpectedArrayType, isExpectedIntType)
import Compiler.Frontend.SrcLoc
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified
import Compiler.Intermediate.Unique
import Data.Extensible
import Data.Extensible.Effect
import RIO
import RIO.List qualified as List
import RIO.List.Partial qualified as Partial
import RIO.Map qualified as Map
import RIO.Text qualified as T
import Test.Hspec

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
      Left (L _ e) -> expectationFailure . T.unpack $ textDisplay e
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
      Right ty -> expectationFailure . T.unpack $ "should return ExpectedIntType, but got " <> textDisplay ty
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
