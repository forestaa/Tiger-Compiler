{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Env where


import Control.Monad.Except
import Control.Lens ((.~), (%~))
import Data.Extensible

import RIO
import qualified RIO.List as L
import qualified RIO.Map as Map

import SrcLoc



type Id = String
type LId = RealLocated String
unLId :: LId -> Id
unLId (L _ id) = id

type EnvError = String
data EnvOp = BeginScope | Push Id deriving (Eq, Show)
newtype Env a = Env (Record '["stack" :> [EnvOp], "env" :> Map.Map Id [a]]) deriving (Eq, Show)

empty :: Env a
empty = Env $ #stack @= [] <: #env @= Map.empty <: nil

insert :: LId -> a -> Env a -> Env a
insert (L _ id) a (Env env) = Env $ env & #stack %~ (:) (Push id) & #env %~ Map.adjust ((:) a) id

lookup :: MonadError EnvError m => LId -> Env a -> m a
lookup (L loc id) (Env env) = case (env ^. #env) Map.!? id of
  Just [] -> throwError $ concat [show loc, ": not in scope: ", show id]
  Just (a:_) -> return a
  Nothing -> throwError $ show loc ++ ": undefined: " ++ show id

beginScope :: Env a -> Env a
beginScope (Env env) = Env $ env & #stack %~ (:) BeginScope

endScope :: MonadError EnvError m => Env a -> m (Env a)
endScope = return
-- endScope (Env env) = case L.uncons (env ^. #stack) of
--     Just ((Push id), stack) -> case L.tailMaybe ((env ^. #env) Map.!? id) of
--         Just rest -> endScope . Env $ env & #stack .~ stack & #env %~ Map.adjust (const rest) id

-- endScope :: Env a -> Env a
-- endScope (Env []) = Env []
-- endScope (Env (_:envs)) = Env envs