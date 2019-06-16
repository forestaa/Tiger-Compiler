{-# LANGUAGE TemplateHaskell #-}

module AbstSyntax.TH (mkFAbstSyntaxes) where

import RIO
import RIO.Char (toLower)
import qualified RIO.List as List

import Language.Haskell.TH
import Language.Haskell.TH.Syntax


-- Functor(?) f traverse declarations of syntax tree
-- e.g.) RealLocated + e = RealLocated e
mkFAbstSyntaxes :: Name -> Name -> [Name] -> Q [Dec]
mkFAbstSyntaxes f wrapF = fmap concat . traverse (mkFAbstSyntax f wrapF)

-- validation of f
-- f must have 1 constructor and 1 type variable
mkFAbstSyntax :: Name -> Name -> Name -> Q [Dec]
mkFAbstSyntax f wrapF syntax = do
  finfo <- reify f
  case finfo of
    TyConI (DataD _ _ [_] _ [con] _) -> mkFAbstSyntaxDefs f wrapF syntax con
    TyConI (DataD _ _ _ _ _ _) -> fail ("mkFAbstSyntax: The functor should have only one type argument and only one constructor: " ++ show f)
    a -> fail $ "mkFAbstSyntax: This pattern is not implemented"  ++ show a


-- costruct syntax tree and functions
mkFAbstSyntaxDefs :: Name -> Name -> Name -> Con -> Q [Dec]
mkFAbstSyntaxDefs f wrapF syntax fcon = do
  dataInfo <- reify syntax
  case dataInfo of
    TyConI (DataD _ _ vars _ cons _) -> do
      unless (null vars) $ fail ("mkFAbstSyntaxDefs: The Type should not have any type arguments: " ++ show syntax)
      let (datadec, synonym) = mkFDataSyn f syntax cons
      (unFFun, unFSig) <- mkUnF syntax fcon cons
      (sToFSFun, sToFSSig) <- mkSyntaxToFSyntax syntax wrapF cons
      return [datadec, synonym, unFFun, unFSig, sToFSFun, sToFSSig]
    TyConI (TySynD _ _ _) -> do
      x <- newName "x"
      let fsyntax = mkFSyntaxName syntax
          synonym = mkFSynonym fsyntax f syntax

          unF = mkUnFName syntax
          sig = mkUnFSig unF fsyntax syntax
          fun = mkUnFSyn unF fcon x
      return [synonym, sig, fun]
    a -> fail $ "mkFAbstSyntaxDefs: This pattern is not implemented: " ++ show a

mkNameWith :: (String -> String) -> Name -> Name
mkNameWith f = mkName . f . nameBase
reMkName :: Name -> Name
reMkName = mkNameWith id
wrappedName :: String -> String
wrappedName = (:) 'L'
mkFSyntaxName :: Name -> Name
mkFSyntaxName = mkNameWith wrappedName
mkFSyntaxName' :: Name -> Name
mkFSyntaxName' = mkNameWith $ (++ "'") . wrappedName
mkUnFName :: Name -> Name
mkUnFName = mkNameWith $ ("un" ++) . wrappedName
mkSyntaxToFSyntaxName :: Name -> Name
mkSyntaxToFSyntaxName = mkNameWith $ \s -> fmap toLower s ++ "To" ++ wrappedName s

renameCon :: Con -> Con
renameCon (NormalC con args) = NormalC (reMkName con) (renameBangType <$> args)
renameCon (RecC con args) = RecC (reMkName con) (renameVarBangType <$> args)

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

mkFDataSyn :: Name -> Name -> [Con] -> (Dec, Dec)
mkFDataSyn f syntax cons = (mkFData' fsyntax' cons', mkFSynonym fsyntax f fsyntax')
  where
    fsyntax  = mkFSyntaxName syntax
    fsyntax' = mkFSyntaxName' syntax
    cons'   = renameCon <$> cons

mkUnF :: Name -> Con -> [Con] -> Q (Dec, Dec)
mkUnF syntax fcon cons = do
  let unF = mkUnFName syntax
      fsyntax  = mkFSyntaxName syntax
      sig = mkUnFSig unF fsyntax syntax
  unFFun <- mkUnFFun unF fcon cons
  return (unFFun, sig)
    
mkUnFSig :: Name -> Name -> Name -> Dec
mkUnFSig unF fsyntax syntax = SigD unF (AppT (AppT ArrowT (ConT fsyntax)) (ConT syntax))

mkUnFSyn :: Name -> Con -> Name -> Dec
mkUnFSyn unF con x = FunD unF [Clause [mkUnFPat (VarP x) con] (NormalB (VarE x)) []]

mkUnFPat :: Pat -> Con -> Pat
mkUnFPat p (NormalC con args) = ConP con (mkUnFPatArg . snd <$> args)
  where
    mkUnFPatArg (VarT _) = p
    mkUnFPatArg _ = WildP

mkUnFFun :: Name -> Con -> [Con] -> Q Dec
mkUnFFun unF fcon syncons = funD unF (mkUnFClause fcon <$> syncons)

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


mkSyntaxToFSyntax :: Name -> Name -> [Con] -> Q (Dec, Dec)
mkSyntaxToFSyntax syntax wrapF syncons = do
  let f = mkSyntaxToFSyntaxName syntax
      sig = mkSyntaxToFSyntaxSig f syntax
  fun <- mkSyntaxToFSyntaxFun f wrapF syncons
  return (fun, sig)

mkSyntaxToFSyntaxSig ::  Name -> Name -> Dec
mkSyntaxToFSyntaxSig f syntax = SigD f (AppT (AppT ArrowT (ConT syntax)) (ConT fsyntax))
  where
    fsyntax = mkFSyntaxName syntax

mkSyntaxToFSyntaxFun :: Name -> Name -> [Con] -> Q Dec
mkSyntaxToFSyntaxFun f wrapF syncons = funD f (mkSyntaxToFSyntaxClause wrapF <$> syncons)

mkSyntaxToFSyntaxClause :: Name -> Con -> Q Clause
mkSyntaxToFSyntaxClause wrapF con = do
  (pat, exp) <- mkSyntaxToFSyntaxPatExp wrapF con
  return $ Clause [pat] (NormalB exp) []

mkSyntaxToFSyntaxPatExp :: Name -> Con -> Q (Pat, Exp)
mkSyntaxToFSyntaxPatExp wrapF (NormalC con args) = do
  (pats, exps) <- List.unzip <$> traverse (mkSyntaxToFSyntaxPatExpUnit . snd) args
  return (ConP con pats, AppE (VarE wrapF) (foldl' AppE (ConE $ reMkName con) exps))
mkSyntaxToFSyntaxPatExp wrapF (RecC con args) = do
  (pats, exps) <- List.unzip <$> traverse (mkSyntaxToFSyntaxPatExpUnit . thd) args
  return (ConP con pats, AppE (VarE wrapF) (foldl' AppE (ConE $ reMkName con) exps))
  where
    thd (_,_,z) = z

mkSyntaxToFSyntaxPatExpUnit :: Type -> Q (Pat, Exp)
mkSyntaxToFSyntaxPatExpUnit (ConT con)
  | con == ''Int || con == ''String || con == ''Bool = do
    x <- newName "x"
    return (VarP x, VarE x)
  | otherwise = do
    let syntaxToFSyntax = mkSyntaxToFSyntaxName con
    x <- newName "x"
    return (VarP x, AppE (VarE syntaxToFSyntax) (VarE x))
mkSyntaxToFSyntaxPatExpUnit (AppT f (ConT con)) = do
  let syntaxToFSyntax = mkSyntaxToFSyntaxName con
  x <- newName "x"
  return (VarP x, AppE (AppE (VarE 'fmap) (VarE syntaxToFSyntax)) (VarE x))
