{-@ LIQUID "--no-termination-check" @-}
{-@ LIQUID "--exactdc" @-}
{-# LANGUAGE TupleSections #-}
module Linear.Eval where

import Control.Monad.Except
import RIO
import System.IO

import qualified Env as E
import Linear.Syntax


type EnvL = E.Env Int
{-@ type EnvLX e = EnvX Int e @-}

run :: EnvL -> Stm -> IO (Either String ((), EnvL))
run env stm = runExceptT $ eval stm env

{-@ eval :: Stm -> e: Env Int -> ExceptT String IO ((), EnvLX e)  @-}
eval :: Stm -> EnvL -> ExceptT String IO ((), EnvL)
eval (CompoundStm s s') env = do
  (_, env') <- eval s env
  eval s' env'
eval (AssignStm x e) env = do
  (v, env') <- evalExp e env
  return ((), E.insert x v env')
eval (PrintStm es) env = do
  (vs, env') <- evalList es env
  liftIO $ (, env') <$> print vs
  where
    evalList [] env = return ([], env)
    evalList (e:es) env = do
      (v, env') <- evalExp e env
      (\(vs, env'') -> (v:vs, env'')) <$> evalList es env'

{-@ evalExp :: Exp -> e: Env Int -> ExceptT String IO (Int, EnvLX e)  @-}
evalExp :: Exp -> EnvL -> ExceptT String IO (Int, EnvL)
evalExp (Id x) env = case E.lookup x env of
    Just v  -> return (v, env)
    Nothing -> throwError ("Exception: undefined variable: " ++ x)
evalExp (Num v) env = return (v, env)
evalExp (Plus  e1 e2) env = do
  (v1, env') <- evalExp e1 env
  (v2, env'') <- evalExp e2 env'
  return (v1 + v2, env'')
evalExp (Minus e1 e2) env = do
  (v1, env') <- evalExp e1 env
  (v2, env'') <- evalExp e2 env'
  return (v1 - v2, env'')
evalExp (Times e1 e2) env = do
  (v1, env') <- evalExp e1 env
  (v2, env'') <- evalExp e2 env'
  return (v1 * v2, env'')
evalExp (Div   e1 e2) env = do
  (v1, env') <- evalExp e1 env
  (v2, env'') <- evalExp e2 env'
  if v2 == 0
    then throwError "Exception: division by zero"
    else return (v1 `div` v2, env'')
evalExp (ESeq stm e) env = do
  -- let env' = beginScope env
  let env' = E.beginScope env
  (_, env'') <- eval stm env'
  (r, env''') <- evalExp e env''
  -- let env'''' = endScope env'''
  let env'''' = E.endScope env'''
  return (r, env'''')

-- test = endScope empty

-- older code
-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedLabels      #-}
-- {-# LANGUAGE TypeApplications      #-}
-- {-# LANGUAGE TypeOperators         #-}

-- import           Data.Extensible
-- import           Data.Extensible.Effect.Default
-- import qualified RIO.Map as Map

-- type IODef = "IO" >: IO
-- runIODef :: Eff '[IODef] r -> IO r
-- runIODef = retractEff

-- type Env = Map.Map String Int
-- run :: Env -> Stm -> IO (Either String (), Env)
-- run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ eval stm) env

-- eval :: Stm -> Eff '[EitherDef String, "env" >: State Env, IODef] ()
-- eval (CompoundStm s s') = eval s >> eval s'
-- eval (AssignStm x e) = do
--   v <- evalExp e
--   modifyEff #env (Map.insert x v)
-- eval (PrintStm es) = do
--   vs <- mapM evalExp es
--   liftIO $ print vs

-- evalExp :: Exp -> Eff '[EitherDef String, "env" >: State Env, IODef] Int
-- evalExp (Id x) = do
--   env <- getEff #env
--   case env Map.!? x of
--     Just v  -> return v
--     Nothing -> throwError ("undefined variable: " ++ x)
-- evalExp (Num v) = return v
-- evalExp (Plus  e1 e2) = (+) <$> evalExp e1 <*> evalExp e2
-- evalExp (Minus e1 e2) = (-) <$> evalExp e1 <*> evalExp e2
-- evalExp (Times e1 e2) = (*) <$> evalExp e1 <*> evalExp e2
-- evalExp (Div   e1 e2) = div <$> evalExp e1 <*> evalExp e2
-- evalExp (ESeq stm e) = eval stm >> evalExp e

