module Env where

import qualified Data.Map.Strict as M

import SrcLoc


type Id = String
type LId = RealLocated String
unLId :: LId -> Id
unLId (L _ id) = id

-- TODO: using list is too naive to represent scope
newtype Env a = Env {unEnv :: [M.Map Id a]} deriving (Eq, Show)

initEnv :: [(Id, a)] -> Env a
initEnv xs = Env [M.fromList xs]

insert :: LId -> a -> Env a -> Env a
insert (L _ id) a (Env (env:envs)) = Env (M.insert id a env : envs)

lookup :: LId -> Env a -> Maybe a
lookup (L _ id) (Env []) = Nothing
lookup (L _ id) (Env (env:_)) = M.lookup id env

beginScope :: Env a -> Env a
beginScope (Env []) = Env []
beginScope (Env envs) = Env (head envs : envs)

endScope :: Env a -> Env a
endScope (Env []) = Env []
endScope (Env (_:envs)) = Env envs