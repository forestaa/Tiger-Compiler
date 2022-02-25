{-# LANGUAGE OverloadedRecordDot #-}

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

import Data.Extensible hiding (getField)
import Data.Extensible.Effect
import Data.List.NonEmpty qualified as NonEmpty (head, singleton, uncons, (<|))
import Id
import RIO hiding (lookup, (%~), (.~))
import RIO.Map qualified as Map

data ScopeOp = Begin | Push Id deriving (Eq, Show)

data Env a = Env {stack :: [ScopeOp], env :: Map.Map Id (NonEmpty a)} deriving (Eq, Show)

empty :: Env a
empty = Env {stack = [], env = Map.empty}

insert :: Id -> a -> Env a -> Env a
insert id a env = env {stack = Push id : env.stack, env = Map.alter f id (env.env)}
  where
    f Nothing = Just $ NonEmpty.singleton a
    f (Just as) = Just $ a NonEmpty.<| as

lookup :: Id -> Env a -> Maybe a
lookup id env = case (env.env) Map.!? id of
  Just as -> pure $ NonEmpty.head as
  Nothing -> Nothing

adjust :: (a -> a) -> Id -> Env a -> Env a
adjust f id env = env {env = Map.adjust (\(a :| as) -> f a :| as) id env.env}

beginScope :: Env a -> Env a
beginScope env = env {stack = Begin : env.stack}

endScope :: Env a -> Env a
endScope env = case env.stack of
  [] -> undefined
  Push id : rest -> endScope $ env {stack = rest, env = Map.update (snd . NonEmpty.uncons) id env.env}
  Begin : rest -> env {stack = rest}

withEnvScope :: Lookup xs k (State (Env a)) => Proxy k -> Eff xs b -> Eff xs b
withEnvScope k t = do
  modifyEff k beginScope
  a <- t
  modifyEff k endScope
  pure a

fromList :: [(Id, a)] -> Env a
fromList = foldr (uncurry insert) empty
