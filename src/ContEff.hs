module ContEff where

import RIO

import Control.Monad.Cont (MonadCont, callCC, ContT(..))
import Control.Monad.Skeleton (boned, MonadView(..))
import Data.Extensible
import Data.Extensible.Internal (here)

type ContDef r m = "Cont" >: ContT r m
pCont :: Proxy "Cont"
pCont = Proxy

callCCEff :: Proxy k -> ((a -> Eff ((k >: ContT r (Eff xs)) : xs) b) -> Eff ((k >: ContT r (Eff xs)) : xs) a) -> Eff ((k >: ContT r (Eff xs)) : xs) a
callCCEff k f = contHead k $ \c -> runContEff (f (\x -> contHead k $ \_ -> c x)) c
  where
    contHead :: Proxy k -> ((a -> Eff xs r) -> Eff xs r) -> Eff ((k >: ContT r (Eff xs)) ': xs) a
    contHead _ c = boned $ Instruction here (ContT c) :>>= return

instance MonadCont (Eff ((ContDef r (Eff xs)) ': xs)) where
  callCC = callCCEff pCont