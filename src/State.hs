module State (
  State,
  runState,
  put,
  get,
  module Control.Monad,
  module Control.Applicative,
) where


import RIO (const)
import Control.Monad
import Control.Applicative


{-@ data State s a <p :: s -> Bool, q :: s -> Bool>  = State (s<p> -> (a, s<q>)) @-}
data State s a = State (s -> (a, s))
{-@ runState :: forall <p :: s -> Bool, q :: s -> Bool>. State <p, q> s a -> s<p> -> (a, s<q>)@-}
runState :: State s a -> s -> (a, s)
runState (State f) = f

{-@ assume const :: v: a -> b -> {u: a | u == v} @-}
{-@ put :: v:s -> State <{\n -> true}, {\n -> v == n}> s () @-}
put :: s -> State s ()
put s = State (const ((), s))

{-@ get :: forall <p :: s -> Bool>. State <p, p> s s@-}
get :: State s s
get = State (\s -> (s, s))

{-@ return :: forall <p:: s -> Bool>. a -> State <p, p> s a @-}
return :: a -> State s a
return a = State (\s -> (a, s))

{-@ (>>=) :: forall <p :: s -> Bool, q :: s -> Bool, r :: s -> Bool>.  State <p, q> s a -> (a -> State <q, r> s b) -> State <p, r> s b @-}
(>>=) :: State s a -> (a -> State s b) -> State s b
m >>= k = State (\s -> let (a, s') = runState m s in runState (k a) s')

{-@ fmap :: forall <p :: s -> Bool, q :: s -> Bool>. (a -> b) -> State <p, q> s a -> State <p, q> s b @-}
fmap :: (a -> b) -> State s a -> State s b
fmap f m = State (\s -> let (a, s') = runState m s in (f a, s'))

instance Functor (State s) where
  fmap = State.fmap
instance Applicative (State s) where
  pure = State.return
  m1 <*> m2 = m2 State.>>= (\a -> State.fmap (\f -> f a) m1)
instance Monad (State s) where
  m >>= k = m State.>>= k