{-@ LIQUID "--exactdc" @-}
{-# LANGUAGE FlexibleContexts #-}

module Env (
  Env,
  EnvX,
  empty,
  insert,
  Env.lookup,
  beginScope,
  endScope,
) where

import RIO hiding (lookup)
import qualified RIO.Map as Map

import Id


data ScopeOp = Begin | Push Id deriving (Eq, Show)
data Env a = Env { stack :: [ScopeOp], env :: Map.Map Id [a]} deriving (Eq, Show)
{-@ data Env a = Env { stack :: [ScopeOp], env :: (Map Id { v: [a] | len v > 0 })} @-}
{-@ type NEEnv a = {e: Env a | scopeNum e > 0} @-}
{-@ type EnvN a N = {e: Env a | scopeNum e = N} @-}
{-@ type EnvX a E = {e: Env a | scopeNum e = scopeNum E} @-}
type EnvX a e = Env a

{-@ measure beginNum @-}
{-@ beginNum :: s: [ScopeOp] -> {v: Nat | (v <= len s) && (len s > 0 && head s == Begin => v == 1 + beginNum (tail s)) && (len s > 0 && not (head s == Begin) => v == beginNum (tail s)) && (len s == 0 => v == 0) } @-}
beginNum :: [ScopeOp] -> Int
beginNum [] = 0
beginNum (Begin:xs) = 1 + beginNum xs
beginNum (_:xs) = beginNum xs
{-@ measure scopeNum @-}
{-@ scopeNum :: e: Env a -> {n: Nat | n == beginNum (stack e) } @-}
scopeNum :: Env a -> Int
scopeNum (Env s _) = beginNum s

{-@ empty :: EnvN a 0 @-}
empty :: Env a
empty = Env [] Map.empty

{-@ insert :: Id -> a -> e:Env a -> EnvN a {scopeNum e} @-}
insert :: Id -> a -> Env a -> Env a
insert id a (Env s e) = Env { stack = Push id : s, env = Map.adjust ((:) a) id e }

lookup :: Id -> Env a -> Maybe a
lookup id (Env _ e) = case Map.lookup id e of
  Just (a:_) -> return a
  Nothing -> Nothing

{-@ beginScope :: e: Env a -> EnvN a {1 + scopeNum e} @-}
beginScope :: Env a -> Env a
beginScope e = e { stack = Begin : stack e }

{-@ endScope :: e: NEEnv a -> EnvN a {scopeNum e - 1} / [len (stack e)] @-}
endScope :: Env a -> Env a
endScope (Env (Push id:rest) e) = endScope $ Env rest (Map.update pop id e)
  where
    pop []  = Nothing
    pop [_] = Nothing
    pop (_:as) = Just as
endScope (Env (Begin:rest) e) = Env rest e

