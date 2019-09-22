module Env (
  Env,
  empty,
  insert,
  lookup,
  adjust,
  beginScope,
  endScope,
  withEnvScope,
  fromList,
) where

import RIO hiding (lookup)
import qualified RIO.Map as Map

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
  Just (a:_) -> pure a
  Nothing -> Nothing

adjust :: (a -> a) -> Id -> Env a -> Env a
adjust f id (Env env) = Env $ env & #env %~ Map.adjust (\(a:as) -> f a : as) id

beginScope :: Env a -> Env a
beginScope (Env env) = Env $ env & #stack %~ (:) Begin

endScope :: Env a -> Env a
endScope (Env env) = case env ^. #stack of
  Push id:rest -> endScope . Env $ env & #stack .~ rest & #env %~ Map.update pop id
  Begin:rest -> Env $ env & #stack .~ rest
  where
    pop [_] = Nothing
    pop (_:as) = Just as

withEnvScope :: Lookup xs k (State (Env a)) => Proxy k -> Eff xs b -> Eff xs b
withEnvScope k t = do
  modifyEff k beginScope
  a <- t
  modifyEff k endScope
  pure a

fromList :: [(Id, a)] -> Env a
fromList = foldr (uncurry insert) empty
