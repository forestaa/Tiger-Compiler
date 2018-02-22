{-# LANGUAGE TemplateHaskell #-}

module AbstSyntax.TH where

import Data.List
import Control.Monad
import Control.Arrow
import Language.Haskell.TH
import Language.Haskell.TH.Syntax



deriveFAbstSyntaxes :: Name -> [Name] -> Q [Dec]
deriveFAbstSyntaxes f = fmap concat . traverse (deriveFAbstSyntax f)

deriveFAbstSyntax :: Name -> Name -> Q [Dec]
deriveFAbstSyntax f syntax = do
  finfo <- reify f
  case finfo of
    TyConI (DataD _ _ vars _ cons _) -> do
      when (length vars /= 1 && length cons /= 1) $ fail "deriveFAbstSyntax: The functor should have only one type argument and only one constructor"
      deriveFAbstSyntaxData f syntax (head cons)
    a -> fail $ "deriveFAbstSyntax: This pattern is not implemented"  ++ show a


deriveFAbstSyntaxData :: Name -> Name -> Con -> Q [Dec]
deriveFAbstSyntaxData f syntax con = do
  dataInfo <- reify syntax
  case dataInfo of
    TyConI (DataD _ _ vars _ cons _) -> do
      unless (null vars) $ fail "deriveFAbstSyntaxData: The Data should not have any type arguments"
      let fsyntax = renameName' syntax
          cons'   = renameCon <$> cons :: [Con]
          datadec = deriveData fsyntax cons'
          synonym = deriveSynonym (renameName syntax) f fsyntax

          unF = renameFunName syntax
          sig = deriveUnFSig unF (renameName syntax) syntax
      fun <- deriveUnF unF con cons
      return [datadec, synonym, sig, fun]
    TyConI (TySynD _ _ _) -> do
      x <- newName "x"
      let fsyntax = renameName syntax
          synonym = deriveSynonym fsyntax f syntax

          unF = mkName $ "un" ++ nameBase fsyntax
          sig = deriveUnFSig unF fsyntax syntax
          fun = deriveUnSynF unF con x
      return [synonym, sig, fun]
    a -> fail $ "deriveFAbstSyntaxData: This pattern is not implemented: " ++ show a

renameName :: Name -> Name
renameName = mkName . (:) 'L' . nameBase
renameName' :: Name -> Name
renameName' name = mkName $ nameBase (renameName name) ++ "'"
renameFunName :: Name -> Name
renameFunName = mkName . ("un" ++) . nameBase . renameName

renameCon :: Con -> Con
renameCon (NormalC name typeArgs) = NormalC (mkName $ nameBase name) (renameBangType <$> typeArgs)
renameCon (RecC name typeArgs) = RecC (mkName $ nameBase name) (renameVarBangType <$> typeArgs)

renameBangType :: BangType -> BangType
renameBangType (bang, t) = (bang, renameType t)

renameVarBangType :: VarBangType -> VarBangType
renameVarBangType (field, bang, t) = (field, bang, renameType t)

renameType :: Type -> Type
renameType t@(ConT name)
  | name == ''Int || name == ''String || name == ''Bool = t
  | otherwise = ConT $ renameName name
renameType (AppT f t) = AppT f $ renameType t

deriveData :: Name -> [Con] -> Dec
deriveData syntax cons = DataD [] syntax [] Nothing cons [DerivClause Nothing [ConT ''Show, ConT ''Eq]]

deriveSynonym :: Name -> Name -> Name -> Dec
deriveSynonym fsyntax f syntax = TySynD fsyntax [] (AppT (ConT f) (ConT syntax))

deriveUnFSig :: Name -> Name -> Name -> Dec
deriveUnFSig unF fsyntax syntax = SigD unF (AppT (AppT ArrowT (ConT fsyntax)) (ConT syntax))


deriveUnSynF :: Name -> Con -> Name -> Dec
deriveUnSynF unF con x = FunD unF [Clause [deriveUnFPat con (VarP x)] (NormalB (VarE x)) []]

deriveUnFPat :: Con -> Pat -> Pat
deriveUnFPat (NormalC con args) p = ConP con (deriveUnFPatArg p . snd <$> args)

deriveUnFPatArg :: Pat -> Type -> Pat
deriveUnFPatArg p (VarT _) = p
deriveUnFPatArg p _ = WildP


deriveUnF :: Name -> Con -> [Con] -> Q Dec
deriveUnF unF fcon syncons = funD unF (f fcon <$> syncons)

f :: Con -> Con -> Q Clause
f (NormalC fcon args) con = do
  (pat, exp) <- g con
  return $ Clause [ConP fcon $ fmap (deriveUnFPatArg pat . snd) args] (NormalB exp) []

g :: Con -> Q (Pat, Exp)
g (NormalC con args) = do
  (pats, exps) <- unzip <$> traverse (h . snd) args
  return (ConP (mkName $ nameBase con) pats, foldl' AppE (ConE con) exps)
g (RecC con args) = do
  (pats, exps) <- unzip <$> traverse (h . thd) args
  return (ConP (mkName $ nameBase con) pats, foldl' AppE (ConE con) exps)
  where
    thd (_,_,z) = z

h :: Type -> Q (Pat, Exp)
h (ConT con)
  | con == ''Int || con == ''String || con == ''Bool = do
    x <- newName "x"
    return (VarP x, VarE x)
  | otherwise = do
    x <- newName "x"
    let unF = renameFunName con
    return (VarP x, AppE (VarE unF) (VarE x))
h (AppT f (ConT con)) = do
  x <- newName "x"
  let unF = renameFunName con
  return (VarP x, AppE (AppE (VarE 'fmap) (VarE unF)) (VarE x))