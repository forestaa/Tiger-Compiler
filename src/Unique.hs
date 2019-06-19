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
  pure $ Unique id

newtype Temp = Temp Unique
newTemp :: Lookup xs k (State Unique) => Proxy k -> Eff xs Temp
newTemp k = Temp <$> getUniqueEff k

makeString :: Temp -> String
makeString (Temp (Unique n)) = "t" ++ show n

data Label = Label String Unique
newLabel :: Lookup xs k (State Unique) => Proxy k -> Eff xs Label
newLabel k = namedLabel k "L"

namedLabel :: Lookup xs k (State Unique) => Proxy k -> String -> Eff xs Label
namedLabel k l = Label l <$> getUniqueEff k
