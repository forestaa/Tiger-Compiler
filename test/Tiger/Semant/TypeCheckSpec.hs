module Tiger.Semant.TypeCheckSpec (spec) where

import           Control.Monad.Trans.Cont
import           Data.Extensible
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
import qualified Tiger.LSyntax as T (valueToLValue)
import qualified Tiger.Syntax as T


spec :: Spec
spec = describe "hoge" $
  it "a[0]" $ do
    let ast = T.valueToLValue $ T.ArrayIndex (T.Id "a") (T.Int 0)
        result = runEff $ do
          id <- getUniqueEff #id
          let arrayTy = TArray $ #range @= TInt <: #id @= id <: nil
          insertType "a" arrayTy
          evalContT (typeCheckValue ast) >>= \case
            Done _ -> throwEff #translateError . dummyRealLocated $ NotImplemented "1"
            Next (Exp _) _ -> throwEff #translateError . dummyRealLocated $ NotImplemented "2"
            Next (Value _) k -> k arrayTy >>= \case
                Done ty -> throwEff #translateError . dummyRealLocated $ NotImplemented $ "3: " ++ show ty
                Next (Value _) _ -> throwEff #translateError . dummyRealLocated $ NotImplemented $ "4"
                Next (Exp _) k -> k TInt >>= \case
                    Done ty -> pure ty
                    _ -> throwEff #translateError . dummyRealLocated $ NotImplemented "5"
    case result of
      Left (L _ e) -> expectationFailure $ show e
      Right ty -> do
        ty `shouldBe` TInt

runEff :: Eff '[
        ("typeEnv" >: State TEnv)
      , ("id" >: UniqueEff)
      , ("translateError" >: EitherEff (RealLocated TranslateError))
      ] a
  -> Either (RealLocated TranslateError) a
runEff = leaveEff . runEitherEff @"translateError" . runUniqueEff @"id" . evalTEnvEff initTEnv
