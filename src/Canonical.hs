module Canonical where

import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)
import Data.Foldable (foldrM)
import IR
import RIO hiding (Const)
import Unique (UniqueEff, newTemp)

linearize :: Lookup xs "temp" UniqueEff => Stm -> Eff xs [Stm]
linearize = fmap seqToList . linearizeStm
  where
    seqToList ((Exp (Const _)) `Seq` s2) = seqToList s2
    seqToList (s1 `Seq` s2) = seqToList s1 ++ seqToList s2
    seqToList (Exp (Const _)) = []
    seqToList s = [s]

linearizeStm :: Lookup xs "temp" UniqueEff => Stm -> Eff xs Stm
linearizeStm (s1 `Seq` s2) = Seq <$> linearizeStm s1 <*> linearizeStm s2
linearizeStm (Jump e labels) = do
  (s, e') <- linearizeExp e
  pure $ s `Seq` Jump e' labels
linearizeStm (CJump op e1 e2 t f) = do
  (s1, e1') <- linearizeExp e1
  (s2, e2') <- linearizeExp e2
  temp <- newTemp
  pure $ s1 `Seq` (Move (Temp temp) e1' `Seq` (s2 `Seq` CJump op (Temp temp) e2' t f))
linearizeStm (Move (Temp temp) (Call f es)) = do
  (s, f') <- linearizeExp f
  (s', es') <- linearizeExps es
  pure (s `Seq` (s' `Seq` (Move (Temp temp) (Call f' es'))))
linearizeStm (Move (Temp temp) e) = do
  (s, e') <- linearizeExp e
  pure $ s `Seq` Move (Temp temp) e'
linearizeStm (Move (Mem e1) e2) = do
  (s1, e1') <- linearizeExp e1
  (s2, e2') <- linearizeExp e2
  pure $ s1 `Seq` (s2 `Seq` (Move (Mem e1') e2'))
linearizeStm (Move (s `ESeq` e1) e2) = linearizeStm $ s `Seq` Move e1 e2
linearizeStm (Exp (Call f es)) = do
  (s, f') <- linearizeExp f
  (s', es') <- linearizeExps es
  pure $ s `Seq` (s' `Seq` Exp (Call f' es'))
linearizeStm (Exp e) = do
  (s, e') <- linearizeExp e
  pure $ s `Seq` Exp e'
linearizeStm s = pure s

linearizeExp :: Lookup xs "temp" UniqueEff => Exp -> Eff xs (Stm, Exp)
linearizeExp (BinOp op e1 e2) = do
  (s1, e1') <- linearizeExp e1
  (s2, e2') <- linearizeExp e2
  temp <- newTemp
  pure (s1 `Seq` (Move (Temp temp) e1' `Seq` s2), BinOp op (Temp temp) e2')
linearizeExp (Mem e) = do
  (s, e') <- linearizeExp e
  pure (s, Mem e')
linearizeExp (ESeq s e) = do
  s' <- linearizeStm s
  (s'', e'') <- linearizeExp e
  pure (s' `Seq` s'', e'')
linearizeExp (Call f es) = do
  (s, f') <- linearizeExp f
  (s', es') <- linearizeExps es
  temp <- newTemp
  pure (s `Seq` (s' `Seq` Move (Temp temp) (Call f' es')), Temp temp)
linearizeExp e = pure (noop, e)

linearizeExps :: Lookup xs "temp" UniqueEff => [Exp] -> Eff xs (Stm, [Exp])
linearizeExps = foldrM f (noop, [])
  where
    f :: Lookup xs "temp" UniqueEff => Exp -> (Stm, [Exp]) -> Eff xs (Stm, [Exp])
    f e (s, es) = do
      (s', e') <- linearizeExp e
      temp <- newTemp
      pure (s' `Seq` (Move (Temp temp) e' `Seq` s), Temp temp : es)

isCommutative :: Stm -> Exp -> Bool
isCommutative _ _ = False
