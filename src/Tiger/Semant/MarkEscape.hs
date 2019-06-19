module Tiger.Semant.MarkEscape (markEscape) where

import RIO
import qualified RIO.List.Partial as List (head)
import Data.Extensible
import Data.Maybe
import qualified Tiger.LSyntax as T

import qualified Env as E
import Id
import SrcLoc

type EscapeEff = '["depth" >: State Int, "env" >: State (E.Env Int)]

markEscape :: T.LExp -> T.LExp
markEscape = leaveEff . flip (evalStateEff @"env") E.empty . flip (evalStateEff @"depth") 0 . traverseExp

withDepthIncre :: Eff EscapeEff a -> Eff EscapeEff a
withDepthIncre t = do
  modifyEff #depth $ (+) 1
  a <- t
  modifyEff #depth $ flip (-) 1
  pure a

validateVarUsage :: [Id] -> Eff EscapeEff a -> Eff EscapeEff ([Bool], a)
validateVarUsage ids t = E.withEnvScope #env $ do
  d <- getEff #depth
  modifyEff #env $ \e -> foldr (`E.insert` d) e ids
  a <- t
  ds <- getsEff #env $ \e -> foldr (\id -> (:) (fromJust (E.lookup id e))) [] ids
  pure ((<) d <$> ds, a)

traverseExp :: T.LExp -> Eff EscapeEff T.LExp
traverseExp (L loc (T.Var v)) = L loc . T.Var <$> traverseValue v
traverseExp (L loc (T.ArrayCreate typeid size init)) = L loc . T.ArrayCreate typeid size <$> traverseExp init
traverseExp (L loc (T.RecordCreate typeid fields)) = L loc . T.RecordCreate typeid <$> mapM traverseFieldAssign fields
traverseExp (L loc (T.FunApply func args)) = L loc . T.FunApply func <$> mapM traverseExp args
traverseExp (L loc (T.Op left lop right)) = (\left right -> L loc $ T.Op left lop right) <$> traverseExp left <*> traverseExp right
traverseExp (L loc (T.Seq exps)) = L loc . T.Seq <$> mapM traverseExp exps
traverseExp (L loc (T.Assign v e)) = L loc <$> (T.Assign <$> traverseValue v <*> traverseExp e)
traverseExp (L loc (T.If cond then' else')) = L loc <$> (T.If <$> traverseExp cond <*> traverseExp then' <*> mapM traverseExp else')
traverseExp (L loc (T.While cond body)) = L loc <$> (T.While <$> traverseExp cond <*> traverseExp body)
traverseExp (L loc (T.For id _ from to body)) = do
  from' <- traverseExp from
  to' <- traverseExp to
  (escs, body') <- validateVarUsage [unLId id] $ traverseExp body
  pure . L loc $ T.For id (List.head escs) from' to' body'
traverseExp (L loc (T.Let [] body)) = L loc . T.Let [] <$> traverseExp body
traverseExp (L loc (T.Let (d:ds) body)) = do
    d' <- traverseDec d
    case d' of
      L loc' (T.VarDec id b t init) -> do
        (escs, lets) <- validateVarUsage [unLId id] $ traverseExp (L loc (T.Let ds body))
        case lets of
          L _ (T.Let decs' body') -> pure . L loc $ T.Let (L loc' (T.VarDec id (b || List.head escs) t init) : decs') body'
      d' -> do
        lets <- traverseExp (L loc (T.Let ds body))
        case lets of
          L _  (T.Let decs' body') -> pure . L loc $ T.Let (d' : decs') body'
traverseExp le = pure le

traverseValue :: T.LValue -> Eff EscapeEff T.LValue
traverseValue (L loc (T.Id id)) = do
  d <- getEff #depth
  modifyEff #env $ E.adjust (max d) (unLId id)
  pure $ L loc (T.Id id)
traverseValue (L loc (T.RecField v id)) = L loc . flip T.RecField id <$> traverseValue v
traverseValue (L loc (T.ArrayIndex v e)) = L loc <$> (T.ArrayIndex <$> traverseValue v <*> traverseExp e)

traverseFieldAssign :: T.LFieldAssign -> Eff EscapeEff T.LFieldAssign
traverseFieldAssign (L loc (T.FieldAssign lid e)) = L loc . T.FieldAssign lid <$> traverseExp e

traverseDec :: T.LDec -> Eff EscapeEff T.LDec
traverseDec (L loc (T.FunDec func args rettype body)) = withDepthIncre $ do
  let ids = map (\(L _ (T.Field id _ _)) -> unLId id) args
  (escs, body') <- validateVarUsage ids (traverseExp body)
  let args' = (\(L loc (T.Field id _ typeid), esc) -> L loc (T.Field id esc typeid)) <$> zip args escs
  pure . L loc $ T.FunDec func args' rettype body'
traverseDec (L loc (T.VarDec id esc t init)) = L loc . T.VarDec id esc t <$> traverseExp init
traverseDec ty = pure ty
