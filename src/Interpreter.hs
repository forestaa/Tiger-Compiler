{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreter where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Extensible
import           Data.Extensible.Effect
import           Data.Extensible.Effect.Default
import qualified Data.Map.Strict                as M
import qualified Tiger                          as T

type IODef = "IO" >: IO
runIODef :: Eff '[IODef] r -> IO r
runIODef = retractEff

run :: T.Env -> T.Stm -> IO (Either String (), T.Env)
run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ interpreter stm) env


-- interpreter :: (MonadState T.Env m, MonadError String m)=> T.Stm -> m ()
-- interpreter :: T.Stm -> Eff '[EitherDef String, StateDef T.Env] ()
-- interpreter :: T.Stm -> Eff '[EitherDef String, "env" >: State T.Env, "io" >: IO] ()
-- interpreter :: (Associate "env" (State T.Env) xs, MonadError String (Eff xs), Associate "io" IO xs) => T.Stm -> Eff xs ()
interpreter :: (Associate "env" (State T.Env) xs, MonadError String (Eff xs), MonadIO (Eff xs)) => T.Stm -> Eff xs ()
interpreter (T.CompoundStm s s') = interpreter s >> interpreter s'
interpreter (T.AssignStm x e) = do
  v <- eval e
  modifyEff #env (M.insert x v)
interpreter (T.PrintStm es) = do
  vs <- mapM eval es
  liftIO $ print vs

-- eval :: (MonadState T.Env m, MonadError String m) => T.Exp -> m T.Value
-- eval :: T.Exp -> Eff '[EitherDef String, StateDef T.Env] T.Value
eval :: (Associate "env" (State T.Env) xs, MonadError String (Eff xs)) => T.Exp -> Eff xs T.Value
eval (T.Id x) = do
  env <- getEff #env
  case M.lookup x env of
    Just v  -> return v
    Nothing -> throwError ("undefined variable: " ++ x)
eval (T.Num v) = return v
eval (T.BiOp e1 op e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ biop op v1 v2


biop :: T.Biop -> T.Value -> T.Value -> T.Value
biop T.Plus  = (+)
biop T.Minus = (-)
biop T.Times = (*)
biop T.Div   = div
