module Compiler.Intermediate.Unique where

import Data.Extensible
import Data.Extensible.Effect
import RIO

newtype Unique = Unique Int deriving (Eq, Ord)

instance Show Unique where
  show (Unique i) = 'u' : show i

type UniqueEff = State Unique

class HasUnique u where
  getUnique :: u -> Unique
  putUnique :: Unique -> u -> u

instance HasUnique Unique where
  getUnique = id
  putUnique = const

instance Lookup xs "unique" Unique => HasUnique (Record xs) where
  getUnique r = r ^. #unique
  putUnique u r = r & #unique .~ u

uniqueSeed :: Unique
uniqueSeed = Unique 0

evalUniqueEff :: Eff ((k >: UniqueEff) ': xs) a -> Eff xs a
evalUniqueEff = flip evalStateEff uniqueSeed

runUniqueEff :: Unique -> Eff ((k >: UniqueEff) ': xs) a -> Eff xs (a, Unique)
runUniqueEff u = flip runStateEff u

getUniqueEff :: (HasUnique u, Lookup xs k (State u)) => Proxy k -> Eff xs Unique
getUniqueEff k = do
  Unique id <- getsEff k getUnique
  modifyEff k $ putUnique (Unique (id + 1))
  pure $ Unique id

putUniqueEff :: Lookup xs k UniqueEff => Proxy k -> Unique -> Eff xs ()
putUniqueEff = putEff

data Temp = Temp String Unique deriving (Eq, Show)

newTemp :: Lookup xs "temp" UniqueEff => Eff xs Temp
newTemp = Temp "t" <$> getUniqueEff #temp

newStringTemp :: String -> Temp
newStringTemp s = Temp s (Unique 0)

makeString :: Temp -> String
makeString (Temp str (Unique n)) = str ++ show n

data Label = Label String Unique deriving (Eq, Ord)

newLabel :: Lookup xs "label" UniqueEff => Eff xs Label
newLabel = namedLabel "L"

namedLabel :: Lookup xs "label" UniqueEff => String -> Eff xs Label
namedLabel l = Label l <$> getUniqueEff #label

instance Show Label where
  show (Label s u) = s ++ show u
