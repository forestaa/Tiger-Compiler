module Env
  ( Env,
    empty,
    insert,
    lookup,
    adjust,
    beginScope,
    endScope,
    withEnvScope,
    fromList,
  )
where

import Control.Lens ((%~), (.~))
import Data.Extensible
import Data.Extensible.Effect
import Id
import RIO hiding (lookup, (%~), (.~))
import RIO.Map qualified as Map
import Data.List.NonEmpty qualified as NonEmpty

data ScopeOp = Begin | Push Id deriving (Eq, Show)

newtype Env a = Env (Record '["stack" :> [ScopeOp], "env" :> Map.Map Id (NonEmpty a)]) deriving (Eq, Show)

empty :: Env a
empty = Env $ #stack @= [] <: #env @= Map.empty <: nil

insert :: Id -> a -> Env a -> Env a
insert id a (Env env) = Env $ env & #stack %~ (:) (Push id) & #env %~ Map.alter f id
  where
    f Nothing = Just $ NonEmpty.singleton a
    f (Just as) = Just $ a NonEmpty.<| as

lookup :: Id -> Env a -> Maybe a
lookup id (Env env) = case (env ^. #env) Map.!? id of
  Just as -> pure $ NonEmpty.head as
  Nothing -> Nothing

adjust :: (a -> a) -> Id -> Env a -> Env a
adjust f id (Env env) = Env $ env & #env %~ Map.adjust (\(a NonEmpty.:| as) -> f a NonEmpty.:| as) id

beginScope :: Env a -> Env a
beginScope (Env env) = Env $ env & #stack %~ (:) Begin

endScope :: Env a -> Env a
endScope (Env env) = case env ^. #stack of
  [] -> undefined
  Push id : rest -> endScope . Env $ env & #stack .~ rest & #env %~ Map.update (snd . NonEmpty.uncons) id
  Begin : rest -> Env $ env & #stack .~ rest

withEnvScope :: Lookup xs k (State (Env a)) => Proxy k -> Eff xs b -> Eff xs b
withEnvScope k t = do
  modifyEff k beginScope
  a <- t
  modifyEff k endScope
  pure a

fromList :: [(Id, a)] -> Env a
fromList = foldr (uncurry insert) empty
