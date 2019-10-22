module Tiger.Semant.TypeCheck where

import           RIO
import           Control.Monad.Trans.Cont
import           Data.Extensible
import           Tiger.Semant.Env
import           Tiger.Semant.Types
import qualified Tiger.LSyntax as T
import           SrcLoc
import           Id


data TranslateError =
    UnknownType Id
  | ExpectedIntType T.LExp Type
  | ExpectedArray T.LValue Type
data Result xs a = Done Type | Next a (Type -> Eff xs (Result xs a))

lookupTypeIdEff :: (Lookup xs "typeEnv" (State TEnv), Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => LId -> Eff xs Type
lookupTypeIdEff (L loc id) = lookupTypeId id >>= maybe (throwEff #translateError . L loc $ UnknownType id) pure
skipName :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => Type -> Eff xs Type
skipName (TName lid) = lookupTypeIdEff lid >>= skipName
skipName a@(TArray arr) = case arr ^. #range of
  TName id -> do
    ty <- skipName (TName id)
    pure . TArray $ set #range ty arr
  _ -> pure a
skipName ty = pure ty
checkInt :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => Type -> T.LExp -> Eff xs ()
checkInt ty e@(L loc _) =
  unless (ty == TInt) . throwEff #translateError . L loc $ ExpectedIntType e ty

typeCheckValue :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => T.LValue -> ContT (Result xs T.LValue) (Eff xs) (Result xs T.LExp)
typeCheckValue (L loc (T.ArrayIndex lv le)) = do
  valueTy <- shiftT $ \k -> pure $ Next lv k
  lift (skipName valueTy) >>= \case
    TArray a -> resetT $ do
      indexTy <- shiftT $ \k -> pure $ Next le k
      lift $ checkInt indexTy le
      pure . Done $ a ^. #range
    ty -> lift . throwEff #translateError . L loc $ ExpectedArray lv ty

