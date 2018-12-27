{-# LANGUAGE TemplateHaskell #-}

module AbstSyntax.TH where

import RIO
import qualified RIO.List as List

import Language.Haskell.TH
import Language.Haskell.TH.Syntax


mkFAbstSyntaxes :: Name -> [Name] -> Q [Dec]
mkFAbstSyntaxes f = fmap concat . traverse (mkFAbstSyntax f)

mkFAbstSyntax :: Name -> Name -> Q [Dec]
mkFAbstSyntax f syntax = do
  finfo <- reify f
  case finfo of
    TyConI (DataD _ _ [_] _ [con] _) -> mkFAbstSyntaxDefs f syntax con
    TyConI (DataD _ _ _ _ _ _) -> fail ("mkFAbstSyntax: The functor should have only one type argument and only one constructor: " ++ show f)
    a -> fail $ "mkFAbstSyntax: This pattern is not implemented"  ++ show a


mkFAbstSyntaxDefs :: Name -> Name -> Con -> Q [Dec]
mkFAbstSyntaxDefs f syntax fcon = do
  dataInfo <- reify syntax
  case dataInfo of
    TyConI (DataD _ _ vars _ cons _) -> do
      unless (null vars) $ fail ("mkFAbstSyntaxDefs: The Type should not have any type arguments: " ++ show syntax)
      let fsyntax  = mkFSyntaxName syntax
          fsyntax' = mkFSyntaxName' syntax
          cons'   = renameCon <$> cons
          datadec = mkFData' fsyntax' cons'
          synonym = mkFSynonym fsyntax f fsyntax'

          unF = mkUnFName syntax
          sig = mkUnFSig unF fsyntax syntax
      fun <- mkUnF unF fcon cons
      return [datadec, synonym, sig, fun]
    TyConI (TySynD _ _ _) -> do
      x <- newName "x"
      let fsyntax = mkFSyntaxName syntax
          synonym = mkFSynonym fsyntax f syntax

          unF = mkUnFName syntax
          sig = mkUnFSig unF fsyntax syntax
          fun = mkUnFSyn unF fcon x
      return [synonym, sig, fun]
    a -> fail $ "mkFAbstSyntaxDefs: This pattern is not implemented: " ++ show a

mkFSyntaxName :: Name -> Name
mkFSyntaxName = mkName . (:) 'L' . nameBase
mkFSyntaxName' :: Name -> Name
mkFSyntaxName' name = mkName $ nameBase (mkFSyntaxName name) ++ "'"
mkUnFName :: Name -> Name
mkUnFName = mkName . ("un" ++) . nameBase . mkFSyntaxName

renameCon :: Con -> Con
renameCon (NormalC con args) = NormalC (mkName $ nameBase con) (renameBangType <$> args)
renameCon (RecC con args) = RecC (mkName $ nameBase con) (renameVarBangType <$> args)

renameBangType :: BangType -> BangType
renameBangType (bang, t) = (bang, renameType t)

renameVarBangType :: VarBangType -> VarBangType
renameVarBangType (field, bang, t) = (field, bang, renameType t)

renameType :: Type -> Type
renameType t@(ConT syntax)
  | syntax == ''Int || syntax == ''String || syntax == ''Bool = t
  | otherwise = ConT $ mkFSyntaxName syntax
renameType (AppT f t) = AppT f $ renameType t

mkFData' :: Name -> [Con] -> Dec
mkFData' fsyntax' cons = DataD [] fsyntax' [] Nothing cons [DerivClause Nothing [ConT ''Show, ConT ''Eq]]

mkFSynonym :: Name -> Name -> Name -> Dec
mkFSynonym fsyntax f syntax = TySynD fsyntax [] (AppT (ConT f) (ConT syntax))

mkUnFSig :: Name -> Name -> Name -> Dec
mkUnFSig unF fsyntax syntax = SigD unF (AppT (AppT ArrowT (ConT fsyntax)) (ConT syntax))

mkUnFSyn :: Name -> Con -> Name -> Dec
mkUnFSyn unF con x = FunD unF [Clause [mkUnFPat (VarP x) con] (NormalB (VarE x)) []]

mkUnFPat :: Pat -> Con -> Pat
mkUnFPat p (NormalC con args) = ConP con (mkUnFPatArg p . snd <$> args)

mkUnFPatArg :: Pat -> Type -> Pat
mkUnFPatArg p (VarT _) = p
mkUnFPatArg p _ = WildP

mkUnF :: Name -> Con -> [Con] -> Q Dec
mkUnF unF fcon syncons = funD unF (mkUnFClause fcon <$> syncons)

mkUnFClause :: Con -> Con -> Q Clause
mkUnFClause fcon con = do
  (pat, exp) <- mkUnFPatExp con
  return $ Clause [mkUnFPat pat fcon] (NormalB exp) []

mkUnFPatExp :: Con -> Q (Pat, Exp)
mkUnFPatExp (NormalC con args) = do
  (pats, exps) <- List.unzip <$> traverse (mkUnFPatExpUnit . snd) args
  return (ConP (mkName $ nameBase con) pats, foldl' AppE (ConE con) exps)
mkUnFPatExp (RecC con args) = do
  (pats, exps) <- List.unzip <$> traverse (mkUnFPatExpUnit . thd) args
  return (ConP (mkName $ nameBase con) pats, foldl' AppE (ConE con) exps)
  where
    thd (_,_,z) = z
mkUnFPatExp c = fail $ "mkUnFPatExp: This pattern is not implemented: " ++ show c

mkUnFPatExpUnit :: Type -> Q (Pat, Exp)
mkUnFPatExpUnit (ConT con)
  | con == ''Int || con == ''String || con == ''Bool = do
    x <- newName "x"
    return (VarP x, VarE x)
  | otherwise = do
    x <- newName "x"
    let unF = mkUnFName con
    return (VarP x, AppE (VarE unF) (VarE x))
mkUnFPatExpUnit (AppT f (ConT con)) = do
  x <- newName "x"
  let unF = mkUnFName con
  return (VarP x, AppE (AppE (VarE 'fmap) (VarE unF)) (VarE x))
mkUnFPatExpUnit t = fail $ "mkUnFPatExpUnit: This pattern is not implemented: " ++ show t