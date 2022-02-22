module Linear.Eval where

import RIO

import Control.Monad.Except
import Data.Extensible
import Data.Extensible.Effect
import Data.Extensible.Effect.Default

import qualified Env as E
import Linear.Syntax

type IODef = "IO" >: IO
runIODef :: Eff '[IODef] r -> IO r
runIODef = retractEff

type EnvL = E.Env Int
type Output = [String]
run :: EnvL -> Stm -> (Either String (), Output)
run env stm = leaveEff . flip (runStateEff @"output") [] $ flip (evalStateEff @"env") env . runEitherDef $ eval stm
runInit :: Stm -> (Either String (), Output)
runInit = run E.empty

eval :: Stm -> Eff '[EitherDef String, "env" >: State EnvL, "output" >: State Output] ()
eval (CompoundStm s s') = eval s >> eval s'
eval (AssignStm x e) = do
  v <- evalExp e
  modifyEff #env (E.insert x v)
eval (PrintStm es) = do
  vs <- mapM evalExp es
  modifyEff #output $ flip (++) (map show vs)

evalExp :: Exp -> Eff '[EitherDef String, "env" >: State EnvL, "output" >: State Output] Int
evalExp (Id x) = do
  env <- getEff #env
  case E.lookup x env of
    Just v  -> pure v
    Nothing -> throwError ("undefined variable: " ++ x)
evalExp (Num v) = pure v
evalExp (Plus  e1 e2) = (+) <$> evalExp e1 <*> evalExp e2
evalExp (Minus e1 e2) = (-) <$> evalExp e1 <*> evalExp e2
evalExp (Times e1 e2) = (*) <$> evalExp e1 <*> evalExp e2
evalExp (Div   e1 e2) = div <$> evalExp e1 <*> evalExp e2
evalExp (ESeq stm e) = E.withEnvScope #env $ eval stm >> evalExp e
