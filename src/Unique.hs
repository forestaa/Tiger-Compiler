module Unique where

import RIO

import Data.Extensible


newtype Unique = Unique Int deriving (Eq, Show)
type UniqueEff = State Unique

runUniqueEff :: Eff ((k >: UniqueEff) ': xs) a -> Eff xs a
runUniqueEff = flip evalStateEff (Unique 0)

getUniqueEff :: Lookup xs k (State Unique) => Proxy k -> Eff xs Unique
getUniqueEff k = do
  Unique id <- getEff k
  putEff k (Unique (id + 1))
  return $ Unique id
