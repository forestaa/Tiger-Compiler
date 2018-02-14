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

import qualified Linear.Syntax                  as L
import SrcLoc

type IODef = "IO" >: IO
runIODef :: Eff '[IODef] r -> IO r
runIODef = retractEff

run :: L.Env -> L.LStm -> IO (Either String (), L.Env)
run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ interpreter stm) env


interpreter :: L.LStm -> Eff '[EitherDef String, "env" >: State L.Env, IODef] ()
interpreter (L _ (L.CompoundStm s s')) = interpreter s >> interpreter s'
interpreter (L _ (L.AssignStm x e)) = do
  v <- eval e
  modifyEff #env (M.insert x v)
interpreter (L _ (L.PrintStm es)) = do
  vs <- mapM eval es
  liftIO $ print vs

eval :: L.LExp -> Eff '[EitherDef String, "env" >: State L.Env, IODef] L.Value
eval (L _ (L.Id x)) = do
  env <- getEff #env
  case M.lookup x env of
    Just v  -> return v
    Nothing -> throwError ("undefined variable: " ++ x)
eval (L _ (L.Num v)) = return v
eval (L _ (L.Plus  e1 e2)) = (+) <$> eval e1 <*> eval e2
eval (L _ (L.Minus e1 e2)) = (-) <$> eval e1 <*> eval e2
eval (L _ (L.Times e1 e2)) = (*) <$> eval e1 <*> eval e2
eval (L _ (L.Div   e1 e2)) = div <$> eval e1 <*> eval e2
eval (L _ (L.ESeq stm e)) = interpreter stm >> eval e
