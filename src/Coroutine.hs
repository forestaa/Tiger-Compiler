module Coroutine where

import RIO

type family Coroutine (m :: * -> *) (xs :: [*]) (r :: *) where
  Coroutine m '[] r = m r
  Coroutine m ((a, b) ': xs) r = m (a, b -> Coroutine m xs r)

yield :: forall xs m a b r. Monad m => a -> (b -> Coroutine m xs r) -> Coroutine m ((a, b) ': xs) r
yield value cont = pure (value, cont)
{-# INLINE yield #-}

