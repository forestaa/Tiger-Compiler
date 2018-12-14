{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Tiger.Typing where

-- import Control.Monad
-- import Control.Monad.Except
-- import Data.Extensible
-- import Data.Extensible.Effect
-- import Data.Extensible.Effect.Default

-- import Env
-- import SrcLoc
-- import qualified Tiger.LSyntax as T


-- data Type = TNil | TUnit | TInt | TString | TRecord Int [(Id, Type)] | TArray Int Type | TName String (Maybe Type)
-- data Var = Var Type | Fun {args :: [Type], ret :: Type}

-- type TEnv = Env Type
-- initTEnv :: TEnv
-- initTEnv = initEnv [("string", TString), ("int", TInt)]
-- evalTEnvEff :: Eff (("type" >: State TEnv) ': xs) a -> Eff xs a
-- evalTEnvEff = flip (evalStateEff @"type") initTEnv
-- type VEnv = Env Var
-- initVEnv :: VEnv
-- initVEnv = initEnv []
-- evalVEnvEff :: Eff (("value" >: State VEnv) ': xs) a -> Eff xs a
-- evalVEnvEff = flip (evalStateEff @"var") initVEnv

-- newtype Typing a = T {unTyping :: Eff '["type" >: State TEnv, "var" >: State VEnv, "id" >: State Int, EitherDef String] a} deriving (Functor, Applicative, Monad, MonadError String)
-- getid :: Typing Int
-- getid = do
--   id <- getEff #"id"
--   putEff #"id" (id + 1)
--   return id
-- runTyping :: Typing a -> Either String a
-- runTyping t = leaveEff . runEitherDef . flip (evalStateEff @"id") 0 . evalVEnvEff . evalTEnvEff $ unTyping t

-- -- type IRType = ((), Type)
-- typingExp :: T.LExp -> Typing Type
-- typingExp (L _ T.Nil) = return TNil
-- typingExp (L _ (T.Int _)) = return TInt
-- typingExp (L _ (T.String _)) = return TString
-- typingExp (L _ (T.Let decs body)) = do
--   modifyEff #"type" beginScope
--   modifyEff #"var" beginScope
--   sequence $ fmap typingDec decs
--   b <- typingExp body
--   modifyEff #"type" endScope
--   modifyEff #"var" endScope
--   return b
-- -- typingExp (L _ (T.ArrayCreate{})) = return

-- typingValue :: T.LValue -> Typing Type
-- typingValue (L loc (Id id)) = do
--   v <- lookup id <$> getEff #value
--   case v of
--     Just _ ->
--     Nothing -> throwError $ show loc ++ ": undefined variable: " ++ show id
-- typingValue (L loc (RecField x field)) = do
--   v <- lookup x <$> getEff #


-- typingDec :: T.LDec -> Typing ()
-- typingDec (L _ (FunDec _ _ _ _)) = return ()
-- typingDec (L loc (VarDec x (Just typeid) e)) = do
--   t <- lookup typeid <$> getEff #"type"
--   t' <- typingExp e
--   unless (match t t') $ throwError (concat [show loc, ": variable declaration: type and expression doesn't match: type ", show t, ", expression type: ", show t'])
--   modifyEff #"var" $ insert x t
--   where
--     match :: Type -> Type -> Bool
--     match TNil TNil = True
--     match TUnit TUnit = True
--     match TInt TInt = True
--     match TString TString = True
--     match (TRecord id _) (TRecord id' _) = id == id'
--     match TNil (TRecord _ _) = True
--     match (TRecord _ _) TNil = True
--     match (TArray id _) (TArray id _) = id == id'
--     match _ _ = False

-- typingDec (L _ (VarDec x Nothing e)) = do
--   t <- typingExp e
--   modifyEff #"var" $ insert x t
-- typingDec (L _ (TypeDec synonym t) = do
--   ty <- typingType t
--   modifyEff #"type" $ insert synonym ty

-- typingType :: T.Type -> Typing Type
-- typingType (L loc (TypeId id)) = do
--   t <- lookup id <$> getEff #"type"
--   case t of
--     Just ty -> return ty
--     Nothing -> throwError $ show loc ++ ": undefined type: " ++ show (unLId id)
-- typingType (L _ (RecordType fields)) = do
--   typedfields <- traverse typingField fields
--   i <- getid
--   return $ TRecord i typedfields
-- typingType (L loc (ArrayType id)) = do
--   t <- lookup id <$> getEff #"type"
--   case
--   i <- getid
--   return $ TArray i t

-- typingField :: T.Field -> Typing (Id, Type)
-- typingField (T.Field id typeid@(L loc _)) = (,) id . lookup typeid <$> getEff #"type"