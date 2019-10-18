module Tiger.Semant.Env where

import           Data.Extensible
import           RIO

import qualified Env as E
import qualified Frame as F
import           Id
import           Unique

import           Tiger.Semant.Level
import           Tiger.Semant.Types



newtype Access f = Access (Record '["level" >: Level f,"access" >: F.Access f])
data Var f = Var (Record '["type" >: Type, "access" >: Access f])
           | Fun (Record '["label" :> Label, "parent" >: Level f, "domains" :> [Type], "codomain" :> Type])

type TEnv = E.Env Type
type VEnv f = E.Env (Var f)
type HasEnv xs f = (Lookup xs "varEnv" (State (VEnv f)), Lookup xs "typeEnv" (State TEnv))
evalTEnvEff :: TEnv -> Eff (("typeEnv" >: State TEnv) ': xs) a -> Eff xs a
evalTEnvEff = flip (evalStateEff @"typeEnv")
evalVEnvEff :: VEnv f -> Eff (("varEnv" >: State (VEnv f)) ': xs) a -> Eff xs a
evalVEnvEff = flip (evalStateEff @"varEnv")
evalEnvEff :: TEnv -> VEnv f -> Eff (("typeEnv" >: State TEnv) ': ("varEnv" >: State (VEnv f)) ': xs) a -> Eff xs a
evalEnvEff typeEnv varEnv = evalVEnvEff varEnv . evalTEnvEff typeEnv

lookupTypeId :: Lookup xs "typeEnv" (State TEnv) => Id -> Eff xs (Maybe Type)
lookupTypeId id = getsEff #typeEnv $ E.lookup id
lookupVarId :: Lookup xs "varEnv" (State (VEnv f)) => Id -> Eff xs (Maybe (Var f))
lookupVarId id = getsEff #varEnv $ E.lookup id
insertType :: (Lookup xs "typeEnv" (State TEnv)) => Id -> Type -> Eff xs ()
insertType id ty = modifyEff #typeEnv $ E.insert id ty
insertVar :: (Lookup xs "varEnv" (State (VEnv f))) => Id -> Var f -> Eff xs ()
insertVar id v = modifyEff #varEnv $ E.insert id v
withTEnvScope :: (Lookup xs "typeEnv" (State TEnv)) => Eff xs a -> Eff xs a
withTEnvScope = E.withEnvScope #typeEnv
withVEnvScope :: (Lookup xs "varEnv" (State (VEnv f))) => Eff xs a -> Eff xs a
withVEnvScope = E.withEnvScope #varEnv
