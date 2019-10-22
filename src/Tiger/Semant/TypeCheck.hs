module Tiger.Semant.TypeCheck where

import           RIO
import           Control.Monad.Trans.Cont
import           Data.Extensible
import           Tiger.Semant
import           Tiger.Semant.Env
import           Tiger.Semant.Types
import qualified Tiger.LSyntax as T
import           SrcLoc
import           Id


data Result xs = Done Type | Next Hoge (Type -> Eff xs (Result xs))
data Hoge = Value T.LValue | Exp T.LExp

typeCheckValue :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => T.LValue -> ContT (Result xs) (Eff xs) (Result xs)
typeCheckValue (L loc (T.ArrayIndex lv le)) = do
  valueTy <- shiftT $ \k -> pure $ Next (Value lv) k
  lift (skipName valueTy) >>= \case
    TArray a -> do
      indexTy <- shiftT $ \k -> pure $ Next (Exp le) k
      lift $ checkInt indexTy le
      pure . Done $ a ^. #range
    ty -> lift . throwEff #translateError . L loc $ ExpectedArrayType lv ty

