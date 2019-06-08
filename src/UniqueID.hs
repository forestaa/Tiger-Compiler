module UniqueID where

import RIO

import Data.Extensible


newtype Unique = Unique Int deriving (Eq, Show)
type UniqueIDEff = "id" >: State Unique

runUniqueIDEff :: Eff (UniqueIDEff ': xs) a -> Eff xs a
runUniqueIDEff = flip (evalStateEff @"id") (Unique 0)

getIDEff :: Lookup xs "id" (State Unique) => Eff xs Unique
getIDEff = do
  Unique id <- getEff #id
  putEff #id (Unique (id + 1))
  return $ Unique id
