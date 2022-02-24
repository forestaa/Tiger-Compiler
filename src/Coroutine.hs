module Coroutine where

import RIO
import Data.Kind (Type)

type family Coroutine (xs :: [Type]) (m :: Type -> Type) (r :: Type) where
  Coroutine '[] m r = m r
  Coroutine ((a, b) ': xs) m r = m (a, b -> Coroutine xs m r)

yield :: forall xs m a b r. Monad m => a -> (b -> Coroutine xs m r) -> Coroutine ((a, b) ': xs) m r
yield value cont = pure (value, cont)
{-# INLINE yield #-}
