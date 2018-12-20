module Env (
  Env,
  empty,
  insert,
  lookup,
  beginScope,
  endScope,
) where

import RIO hiding (lookup)
import qualified RIO.Map as Map
import qualified RIO.List.Partial as List

import Control.Lens ((.~), (%~))
import Data.Extensible

import Id


data ScopeOp = Begin | Push Id deriving (Eq, Show)
newtype Env a = Env (Record '["stack" :> [ScopeOp], "env" :> Map.Map Id [a]]) deriving (Eq, Show)

empty :: Env a
empty = Env $ #stack @= [] <: #env @= Map.empty <: nil

insert :: Id -> a -> Env a -> Env a
insert id a (Env env) = Env $ env & #stack %~ (:) (Push id) & #env %~ Map.alter f id
  where
    f Nothing = Just [a]
    f (Just as) = Just (a:as)

lookup :: Id -> Env a -> Maybe a
lookup id (Env env) = case (env ^. #env) Map.!? id of
  Just (a:_) -> return a
  Nothing -> Nothing

beginScope :: Env a -> Env a
beginScope (Env env) = Env $ env & #stack %~ (:) Begin

endScope :: Env a -> Env a
endScope (Env env) = case env ^. #stack of
  (Push id):rest -> endScope . Env $ env & #stack .~ rest & #env %~ Map.adjust List.tail id
  Begin:rest -> Env $ env & #stack .~ rest
