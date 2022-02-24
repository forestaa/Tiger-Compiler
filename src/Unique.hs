module Unique where

import Control.Lens ((.~))
import Data.Extensible
import Data.Extensible.Effect
import RIO hiding ((.~))

newtype Unique = Unique Int deriving (Eq, Show)

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

runUniqueEff :: Eff ((k >: UniqueEff) ': xs) a -> Eff xs a
runUniqueEff = flip evalStateEff uniqueSeed

getUniqueEff :: (HasUnique u, Lookup xs k (State u)) => Proxy k -> Eff xs Unique
getUniqueEff k = do
  Unique id <- getsEff k getUnique
  modifyEff k $ putUnique (Unique (id + 1))
  pure $ Unique id

newtype Temp = Temp Unique deriving (Eq, Show)

newTemp :: Lookup xs "temp" UniqueEff => Eff xs Temp
newTemp = Temp <$> getUniqueEff #temp

makeString :: Temp -> String
makeString (Temp (Unique n)) = "t" ++ show n

data Label = Label String Unique deriving (Eq, Show)

newLabel :: Lookup xs "label" UniqueEff => Eff xs Label
newLabel = namedLabel "L"

namedLabel :: Lookup xs "label" UniqueEff => String -> Eff xs Label
namedLabel l = Label l <$> getUniqueEff #label
