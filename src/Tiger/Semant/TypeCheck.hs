module Tiger.Semant.TypeCheck where

import           RIO
import           Control.Monad.Trans.Cont
import           Data.Extensible
import           Tiger.Semant
import           Tiger.Semant.Env
import           Tiger.Semant.Types
import qualified Tiger.LSyntax as T
import           Coroutine
import           SrcLoc
import           Unique
import           Id


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
