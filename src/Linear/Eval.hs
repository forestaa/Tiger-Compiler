module Linear.Eval where

import Control.Monad.Except
import RIO
import System.IO

import qualified Env as E
import Linear.Syntax
import           Data.Extensible
import           Data.Extensible.Effect.Default

type IODef = "IO" >: IO
runIODef :: Eff '[IODef] r -> IO r
runIODef = retractEff

type EnvL = E.Env Int
run :: EnvL -> Stm -> IO (Either String (), EnvL)
run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ eval stm) env

runInit :: Stm -> IO (Either String (), EnvL)
runInit = run E.empty

eval :: Stm -> Eff '[EitherDef String, "env" >: State EnvL, IODef] ()
eval (CompoundStm s s') = eval s >> eval s'
eval (AssignStm x e) = do
  v <- evalExp e
  modifyEff #env (E.insert x v)
eval (PrintStm es) = do
  vs <- mapM evalExp es
  liftIO $ print vs

evalExp :: Exp -> Eff '[EitherDef String, "env" >: State EnvL, IODef] Int
evalExp (Id x) = do
  env <- getEff #env
  case E.lookup x env of
    Just v  -> return v
    Nothing -> throwError ("undefined variable: " ++ x)
evalExp (Num v) = return v
evalExp (Plus  e1 e2) = (+) <$> evalExp e1 <*> evalExp e2
evalExp (Minus e1 e2) = (-) <$> evalExp e1 <*> evalExp e2
evalExp (Times e1 e2) = (*) <$> evalExp e1 <*> evalExp e2
evalExp (Div   e1 e2) = div <$> evalExp e1 <*> evalExp e2
evalExp (ESeq stm e) = do
  modifyEff #env E.beginScope
  eval stm
  v <- evalExp e
  modifyEff #env E.endScope
  return v

