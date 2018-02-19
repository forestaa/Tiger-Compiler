{-# LANGUAGE TemplateHaskell #-}

module AbstSyntax.TH where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import SrcLoc
import Debug.Trace

-- import qualified Tiger.LSyntax as LS
-- import qualified Tiger.Syntax as S

-- $(do
--     info <- reify ''RealLocated
--     runIO $ print info
--     info <- reify ''S.Exp
--     runIO $ print info
--     info <- reify ''LS.LExp'
--     runIO $ print info
--     info <- reify ''LS.LExp
--     runIO $ print info
--     return [])

deriveFAbstSyntaxes :: Name -> [Name] -> Q [Dec]
deriveFAbstSyntaxes f = fmap concat . traverse (deriveFAbstSyntax f)

deriveFAbstSyntax :: Name -> Name -> Q [Dec]
deriveFAbstSyntax f syntax = do
  finfo <- reify f
  case finfo of
    TyConI (DataD _ _ vars _ cons _) -> do
      when (length vars /= 1 && length cons /= 1) $ fail "deriveFAbstSyntax: The functor should have only one type argument and only one constructor"
      datadec <- deriveFAbstSyntaxData syntax
      syndec <- deriveFAbstSynonym f syntax
      return $ syndec : datadec
    a -> fail $ "deriveFAbstSyntax: This pattern is not implemented"  ++ show a


deriveFAbstSyntaxData :: Name -> Q [Dec]
deriveFAbstSyntaxData syntax = do
  dataInfo <- reify syntax
  case dataInfo of
    TyConI (DataD _ _ vars _ cons _) -> do
      unless (null vars) $ fail "deriveFAbstSyntaxData: The Data should not have any type arguments"
      let fsyntax = renameName' syntax
          cons' = renameCon <$> cons :: [Con]
      return [DataD [] fsyntax [] Nothing cons' []]
    TyConI (TySynD _ _ _) -> return []
    a -> fail $ "deriveFAbstSyntaxData: This pattern is not implemented: " ++ show a

renameName :: Name -> Name
renameName = mkName . (:) 'L' . nameBase
renameName' :: Name -> Name
renameName' name = mkName $ nameBase (renameName name) ++ "'"

renameCon :: Con -> Con
renameCon (NormalC name typeArgs) = NormalC (mkName $ nameBase name) (renameBangType <$> typeArgs)
renameCon (RecC name typeArgs) = RecC (mkName $ nameBase name) (renameVarBangType <$> typeArgs)

renameBangType :: BangType -> BangType
renameBangType (bang, t) = (bang, renameType t)

renameVarBangType :: VarBangType -> VarBangType
renameVarBangType (field, bang, t) = (field, bang, renameType t)

renameType :: Type -> Type
renameType t@(ConT name)
  | name == ''Int || name == ''String = t
  | otherwise = ConT $ renameName name
renameType (AppT f t) = AppT f $ renameType t

deriveFAbstSynonym :: Name -> Name -> Q Dec
deriveFAbstSynonym f syntax = tySynD (renameName syntax) [] (appT (conT f) (conT syntax))