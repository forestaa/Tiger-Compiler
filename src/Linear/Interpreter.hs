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

import Linear.Syntax
import SrcLoc

type IODef = "IO" >: IO
runIODef :: Eff '[IODef] r -> IO r
runIODef = retractEff

run :: Env -> Stm -> IO (Either String (), Env)
run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ interpreter stm) env


interpreter :: Stm -> Eff '[EitherDef String, "env" >: State Env, IODef] ()
interpreter (CompoundStm s s') = interpreter s >> interpreter s'
interpreter (AssignStm x e) = do
  v <- eval e
  modifyEff #env (M.insert x v)
interpreter (PrintStm es) = do
  vs <- mapM eval es
  liftIO $ print vs

eval :: Exp -> Eff '[EitherDef String, "env" >: State Env, IODef] Int
eval (Id x) = do
  env <- getEff #env
  case M.lookup x env of
    Just v  -> return v
    Nothing -> throwError ("undefined variable: " ++ x)
eval (Num v) = return v
eval (Plus  e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Minus e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Times e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div   e1 e2) = div <$> eval e1 <*> eval e2
eval (ESeq stm e) = interpreter stm >> eval e
