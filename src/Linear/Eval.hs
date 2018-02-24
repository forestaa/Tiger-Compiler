{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}


module Linear.Eval where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Extensible
import           Data.Extensible.Effect
import           Data.Extensible.Effect.Default
import qualified Data.Map.Strict                as M

import Linear.Syntax

type IODef = "IO" >: IO
runIODef :: Eff '[IODef] r -> IO r
runIODef = retractEff

type Env = M.Map String Int
run :: Env -> Stm -> IO (Either String (), Env)
run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ eval stm) env

eval :: Stm -> Eff '[EitherDef String, "env" >: State Env, IODef] ()
eval (CompoundStm s s') = eval s >> eval s'
eval (AssignStm x e) = do
  v <- evalExp e
  modifyEff #env (M.insert x v)
eval (PrintStm es) = do
  vs <- mapM evalExp es
  liftIO $ print vs

evalExp :: Exp -> Eff '[EitherDef String, "env" >: State Env, IODef] Int
evalExp (Id x) = do
  env <- getEff #env
  case M.lookup x env of
    Just v  -> return v
    Nothing -> throwError ("undefined variable: " ++ x)
evalExp (Num v) = return v
evalExp (Plus  e1 e2) = (+) <$> evalExp e1 <*> evalExp e2
evalExp (Minus e1 e2) = (-) <$> evalExp e1 <*> evalExp e2
evalExp (Times e1 e2) = (*) <$> evalExp e1 <*> evalExp e2
evalExp (Div   e1 e2) = div <$> evalExp e1 <*> evalExp e2
evalExp (ESeq stm e) = eval stm >> evalExp e
