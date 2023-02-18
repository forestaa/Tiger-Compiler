module Compiler.Intermediate.Unique where

import Data.Extensible
import Data.Extensible.Effect
import RIO hiding (String)
import RIO.Text qualified as T (unpack)

newtype Unique = Unique {int :: Int} deriving (Eq, Ord)

instance Show Unique where
  show = T.unpack . textDisplay

instance Display Unique where
  display (Unique i) = "u" <> display i

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

nextUnique :: Unique -> Unique
nextUnique (Unique i) = Unique $ i + 1

evalUniqueEff :: Eff ((k >: UniqueEff) ': xs) a -> Eff xs a
evalUniqueEff = flip evalStateEff uniqueSeed

runUniqueEff :: Unique -> Eff ((k >: UniqueEff) ': xs) a -> Eff xs (a, Unique)
runUniqueEff = flip runStateEff

getUniqueEff :: (HasUnique u, Lookup xs k (State u)) => Proxy k -> Eff xs Unique
getUniqueEff k = do
  unique <- getsEff k getUnique
  modifyEff k . putUnique $ nextUnique unique
  pure unique

putUniqueEff :: Lookup xs k UniqueEff => Proxy k -> Unique -> Eff xs ()
putUniqueEff = putEff

data Temp = Temp {body :: Text, unique :: Unique} deriving (Eq, Ord)

instance Show Temp where
  show = T.unpack . textDisplay

instance Display Temp where
  display temp = display temp.body <> display temp.unique.int

newTemp :: Lookup xs "temp" UniqueEff => Eff xs Temp
newTemp = Temp "t" <$> getUniqueEff #temp

newUniqueTextTemp :: Text -> Temp
newUniqueTextTemp text = Temp text (Unique 0)

data Label = Label {body :: Text, unique :: Unique} deriving (Eq, Ord)

newLabel :: Lookup xs "label" UniqueEff => Eff xs Label
newLabel = namedLabel "L"

namedLabel :: Lookup xs "label" UniqueEff => Text -> Eff xs Label
namedLabel l = Label l <$> getUniqueEff #label

instance Show Label where
  show = T.unpack . textDisplay

instance Display Label where
  display label = display label.body <> display label.unique.int
