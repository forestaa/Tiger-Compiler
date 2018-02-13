{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}


module Linear.Interpreter where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Extensible
import           Data.Extensible.Effect
import           Data.Extensible.Effect.Default
import qualified Data.Map.Strict                as M
import qualified Linear.Linear                  as L

type IODef = "IO" >: IO
runIODef :: Eff '[IODef] r -> IO r
runIODef = retractEff

run :: L.Env -> L.Stm -> IO (Either String (), L.Env)
run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ interpreter stm) env


interpreter :: L.Stm -> Eff '[EitherDef String, "env" >: State L.Env, IODef] ()
interpreter (L.CompoundStm s s') = interpreter s >> interpreter s'
interpreter (L.AssignStm x e) = do
  v <- eval e
  modifyEff #env (M.insert x v)
interpreter (L.PrintStm es) = do
  vs <- mapM eval es
  liftIO $ print vs

eval :: L.Exp -> Eff '[EitherDef String, "env" >: State L.Env, IODef] L.Value
eval (L.Id x) = do
  env <- getEff #env
  case M.lookup x env of
    Just v  -> return v
    Nothing -> throwError ("undefined variable: " ++ x)
eval (L.Num v) = return v
eval (L.BiOp e1 op e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ biop op v1 v2
eval (L.Eseq stm e) = interpreter stm >> eval e

biop :: L.Biop -> L.Value -> L.Value -> L.Value
biop L.Plus  = (+)
biop L.Minus = (-)
biop L.Times = (*)
biop L.Div   = div
