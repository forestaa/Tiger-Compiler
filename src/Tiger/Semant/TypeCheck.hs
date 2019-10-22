module Tiger.Semant.TypeCheck where

import           RIO
import           Control.Monad.Trans.Cont
import           Data.Extensible
import           Tiger.Semant
import           Tiger.Semant.Env
import           Tiger.Semant.Types
import qualified Tiger.LSyntax as T
import           SrcLoc
import           Unique
import           Id


type family Coroutine (m :: * -> *) (xs :: [*]) (r :: *) where
  Coroutine m '[] r = m r
  Coroutine m ((a, b) ': xs) r = m (a, b -> Coroutine m xs r)

yield :: forall xs m a b r. Monad m => a -> (b -> Coroutine m xs r) -> Coroutine m ((a, b) ': xs) r
yield value cont = pure (value, cont)
{-# INLINE yield #-}

typeCheckArrayIndex :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated (T.LValue, T.LExp) -> Coroutine (Eff xs) '[(T.LValue, Type), (T.LExp, Type)] Type
typeCheckArrayIndex (L loc (lv, le)) = yield @'[(T.LExp, Type)] lv \valueTy ->
  skipName valueTy >>= \case
    TArray a -> yield @'[] le \indexTy -> do
      checkInt indexTy le
      pure $ a ^. #range
    ty -> throwEff #translateError . L loc $ ExpectedArrayType lv ty
