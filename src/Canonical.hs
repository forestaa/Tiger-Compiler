module Canonical where

import Data.Extensible (Lookup, type (>:))
import Data.Extensible.Effect (Eff, State, evalStateEff, getEff, getsEff, leaveEff, modifyEff, putEff, runStateEff)
import Data.Foldable (foldrM)
import Data.Graph (Tree (..), dff, graphFromEdges)
import Data.List (init, last)
import IR
import RIO hiding (Const)
import RIO.List (headMaybe)
import Unique (Label, UniqueEff, getUniqueEff, newLabel, newTemp, putUniqueEff)

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
  if s2 `isCommutative` e1'
    then pure $ s1 `Seq` (s2 `Seq` CJump op e1' e2' t f)
    else do
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
  if s2 `isCommutative` e1'
    then pure (s1 `Seq` s2, BinOp op e1' e2')
    else do
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
      if s `isCommutative` e'
        then pure (s' `Seq` s, e' : es)
        else do
          temp <- newTemp
          pure (s' `Seq` (Move (Temp temp) e' `Seq` s), Temp temp : es)

-- TODO
isCommutative :: Stm -> Exp -> Bool
isCommutative (Exp (Const _)) _ = True
isCommutative _ (Name _) = True
isCommutative _ (Const _) = True
isCommutative _ _ = False

data Block = Block {lbl :: Label, statements :: [Stm]} deriving (Eq, Show)

newBlock :: Label -> Block
newBlock label = Block label []

addStatement :: Stm -> Block -> Block
addStatement s b@(Block {statements}) = b {statements = statements ++ [s]}

addStatements :: [Stm] -> Block -> Block
addStatements stms b@(Block {statements}) = b {statements = statements ++ stms}

lastJump :: Block -> Stm
lastJump block = last block.statements

removeLastJump :: Block -> Block
removeLastJump block = case lastJump block of
  Jump _ _ -> block {statements = init block.statements}
  CJump _ _ _ _ _ -> block {statements = init block.statements}
  _ -> block

type BlockEff = State (Maybe Block, [Block])

evalBlockEff :: Eff (k >: State (Maybe Block, [Block]) ': xs) a -> Eff xs a
evalBlockEff effect = evalStateEff effect (Nothing, [])

startCurrentBlockEff :: Lookup xs k BlockEff => Proxy k -> Label -> Eff xs ()
startCurrentBlockEff k label = modifyEff k $ \(block, blocks) -> case block of
  Nothing -> (Just (newBlock label), blocks)
  Just block -> (Just (newBlock label), blocks ++ [block])

addStatementToCurrentBlockEff :: Lookup xs k BlockEff => Proxy k -> Stm -> Eff xs ()
addStatementToCurrentBlockEff k s = modifyEff k . first $ fmap (addStatement s)

isCurrentBlockStartedEff :: Lookup xs k BlockEff => Proxy k -> Eff xs Bool
isCurrentBlockStartedEff k = getsEff k $ isJust . fst

endCurrentBlockEff :: Lookup xs k BlockEff => Proxy k -> Eff xs ()
endCurrentBlockEff k = modifyEff k \(block, blocks) -> case block of
  Nothing -> (Nothing, blocks)
  Just block -> (Nothing, blocks ++ [block])

endBlockEff :: Lookup xs k BlockEff => Proxy k -> Eff xs [Block]
endBlockEff k = do
  (block, blocks) <- getEff k
  putEff k (Nothing, [])
  case block of
    Nothing -> pure blocks
    Just block -> pure $ blocks ++ [block]

basicBlocks :: Lookup xs "label" UniqueEff => [Stm] -> Eff xs ([Block], Label)
basicBlocks stms = do
  unique <- getUniqueEff #label
  let (blocks, newUnique) = leaveEff . flip (runStateEff @"label") unique . evalBlockEff @"block" $ go stms
  putUniqueEff #label newUnique -- workaround: extensible-effect cannot peel action partially
  pure blocks
  where
    go :: (Lookup xs "label" UniqueEff, Lookup xs "block" BlockEff) => [Stm] -> Eff xs ([Block], Label)
    go ((Label label) : stms) = do
      whenM (isCurrentBlockStartedEff #block) do
        addStatementToCurrentBlockEff #block $ Jump (Name label) [label]
        endCurrentBlockEff #block
      startCurrentBlockEff #block label
      go stms
    go (j@(Jump _ _) : stms) = do
      whenM (not <$> isCurrentBlockStartedEff #block) $ startCurrentBlockEff #block =<< newLabel
      addStatementToCurrentBlockEff #block j
      endCurrentBlockEff #block
      go stms
    go (j@(CJump _ _ _ _ _) : stms) = do
      whenM (not <$> isCurrentBlockStartedEff #block) $ startCurrentBlockEff #block =<< newLabel
      addStatementToCurrentBlockEff #block j
      endCurrentBlockEff #block
      go stms
    go (s : stms) = do
      whenM (not <$> isCurrentBlockStartedEff #block) $ startCurrentBlockEff #block =<< newLabel
      addStatementToCurrentBlockEff #block s
      go stms
    go [] = do
      done <- newLabel
      whenM (isCurrentBlockStartedEff #block) do
        addStatementToCurrentBlockEff #block (Jump (Name done) [done])
      (,done) <$> endBlockEff #block

newtype Trace = Trace {blocks :: [Block]} deriving (Eq, Show)

newTrace :: [Block] -> Trace
newTrace = Trace

statements :: Lookup xs "label" UniqueEff => Trace -> Eff xs [Stm]
statements trace = do
  blocks <- processJump trace.blocks
  pure $ concatMap (\block -> Label block.lbl : block.statements) blocks
  where
    processJump :: Lookup xs "label" UniqueEff => [Block] -> Eff xs [Block]
    processJump (currBlock : blocks@(nextBlock : _)) = do
      case lastJump currBlock of
        Jump (Name lbl) _ | lbl == nextBlock.lbl -> (:) (removeLastJump currBlock) <$> processJump blocks
        CJump op e1 e2 t f
          | f == nextBlock.lbl -> (:) currBlock <$> processJump blocks
          | t == nextBlock.lbl -> do
              let newCurrBlock = addStatement (CJump (notRelOp op) e1 e2 f t) $ removeLastJump currBlock
              (:) newCurrBlock <$> processJump blocks
          | otherwise -> do
              f' <- newLabel
              let newCurrBlock = addStatements [CJump op e1 e2 t f', Label f', Jump (Name f) [f]] $ removeLastJump currBlock
              (:) newCurrBlock <$> processJump blocks
        _ -> (:) currBlock <$> processJump blocks
    processJump [currBlock] = case lastJump currBlock of
      CJump op e1 e2 t f -> do
        f' <- newLabel
        let newCurrBlock = addStatements [CJump op e1 e2 t f', Label f', Jump (Name f) [f]] $ removeLastJump currBlock
        pure [newCurrBlock]
      _ -> pure [currBlock]
    processJump [] = pure []

traceSchedule :: Lookup xs "label" UniqueEff => [Block] -> Label -> Eff xs [Stm]
traceSchedule blocks done = do
  let (graph, vertex, _) = graphFromEdges . flip fmap blocks $ \block -> case lastJump block of
        Jump (Name lbl) _ -> (block, block.lbl, [lbl])
        CJump _ _ _ _ false -> (block, block.lbl, [false])
        _ -> undefined
      treeToList (Node block blocks) = block : (maybe [] treeToList $ headMaybe blocks)
      traces = newTrace <$> (fmap (\(a, _, _) -> a) . fmap vertex) <$> treeToList <$> dff graph
  body <- concat <$> mapM statements traces
  pure $ body ++ [Label done]
