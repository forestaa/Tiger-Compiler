module Frontend.Language.Tiger.Semant.Env where

import Data.Extensible
import Data.Extensible.Effect
import Frontend.Env qualified as E
import Frontend.Id
import Intermediate.Frame qualified as F
import Intermediate.Unique
import RIO
import Frontend.Language.Tiger.Semant.Level
import Frontend.Language.Tiger.Semant.Types

data Access f = Access {level :: Level f, access :: F.Access f}

data VarAccess f
  = VarAccess (Access f)
  | FunAccess {label :: Label, parent :: Level f}

data VarType
  = VarType Type
  | FunType {domains :: [Type], codomain :: Type}

type TEnv = E.Env Type

type VAEnv f = E.Env (VarAccess f)

type VTEnv = E.Env VarType

type HasEnv xs f = (Lookup xs "varAccessEnv" (State (VAEnv f)), Lookup xs "varTypeEnv" (State VTEnv), Lookup xs "typeEnv" (State TEnv))

evalTEnvEff :: TEnv -> Eff (("typeEnv" >: State TEnv) ': xs) a -> Eff xs a
evalTEnvEff = flip (evalStateEff @"typeEnv")

evalVAEnvEff :: Eff (("varAccessEnv" >: State (VAEnv f)) ': xs) a -> Eff xs a
evalVAEnvEff = flip (evalStateEff @"varAccessEnv") (E.fromList [])

evalVTEnvEff :: Eff (("varTypeEnv" >: State VTEnv) ': xs) a -> Eff xs a
evalVTEnvEff = flip (evalStateEff @"varTypeEnv") (E.fromList [])

evalEnvEff :: TEnv -> Eff (("typeEnv" >: State TEnv) ': ("varTypeEnv" >: State VTEnv) ': ("varAccessEnv" >: State (VAEnv f)) ': xs) a -> Eff xs a
evalEnvEff typeEnv = evalVAEnvEff . evalVTEnvEff . evalTEnvEff typeEnv

lookupTypeId :: Lookup xs "typeEnv" (State TEnv) => Id -> Eff xs (Maybe Type)
lookupTypeId id = getsEff #typeEnv $ E.lookup id

lookupVarAccess :: Lookup xs "varAccessEnv" (State (VAEnv f)) => Id -> Eff xs (Maybe (VarAccess f))
lookupVarAccess id = getsEff #varAccessEnv $ E.lookup id

lookupVarType :: Lookup xs "varTypeEnv" (State VTEnv) => Id -> Eff xs (Maybe VarType)
lookupVarType id = getsEff #varTypeEnv $ E.lookup id

insertType :: (Lookup xs "typeEnv" (State TEnv)) => Id -> Type -> Eff xs ()
insertType id ty = modifyEff #typeEnv $ E.insert id ty

insertVarAccess :: (Lookup xs "varAccessEnv" (State (VAEnv f))) => Id -> VarAccess f -> Eff xs ()
insertVarAccess id v = modifyEff #varAccessEnv $ E.insert id v

insertVarType :: (Lookup xs "varTypeEnv" (State VTEnv)) => Id -> VarType -> Eff xs ()
insertVarType id v = modifyEff #varTypeEnv $ E.insert id v

beginTEnvScope :: (Lookup xs "typeEnv" (State TEnv)) => Eff xs ()
beginTEnvScope = modifyEff #typeEnv E.beginScope

beginVTEnvScope :: (Lookup xs "varTypeEnv" (State VTEnv)) => Eff xs ()
beginVTEnvScope = modifyEff #varTypeEnv E.beginScope

endTEnvScope :: (Lookup xs "typeEnv" (State TEnv)) => Eff xs ()
endTEnvScope = modifyEff #typeEnv E.endScope

endVTEnvScope :: (Lookup xs "varTypeEnv" (State VTEnv)) => Eff xs ()
endVTEnvScope = modifyEff #varTypeEnv E.endScope

withTEnvScope :: (Lookup xs "typeEnv" (State TEnv)) => Eff xs a -> Eff xs a
withTEnvScope = E.withEnvScope #typeEnv

withVAEnvScope :: (Lookup xs "varAccessEnv" (State (VAEnv f))) => Eff xs a -> Eff xs a
withVAEnvScope = E.withEnvScope #varAccessEnv

withVTEnvScope :: (Lookup xs "varTypeEnv" (State VTEnv)) => Eff xs a -> Eff xs a
withVTEnvScope = E.withEnvScope #varTypeEnv
