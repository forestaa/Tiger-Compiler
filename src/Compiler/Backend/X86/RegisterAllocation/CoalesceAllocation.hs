module Compiler.Backend.X86.RegisterAllocation.CoalesceAllocation (CoalesceAllocation) where

import Compiler.Backend.X86.Arch (Assembly (..), Register (..), callerSaveRegisters, replaceRegister)
import Compiler.Backend.X86.Frame (Access (..), Frame (..), FrameEff, ProcedureX86 (..), allTempRegisters, allocateLocalEff, allocateNonEscapedLocalEff, getAllocatedRegisters, modifyFrameEff, runFrameEff)
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), setDestinations, setSources)
import Compiler.Backend.X86.RegisterAllocation qualified as R (RegisterAllocation (..))
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph (buildInterfereceGraph)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base (InterferenceGraphEdgeLabel, InterferenceGraphNode (vars), Move (destination, source))
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable qualified as Immutable
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable qualified as Mutable
import Compiler.Backend.X86.RegisterAllocation.Coalesce.RegisterAllocator (Allocation, AvailableColors, RegisterAllocator (allocation), allocateEff, getColor, runRegisterAllocatorState)
import Compiler.Intermediate.Frame qualified as F (fp)
import Compiler.Intermediate.Unique qualified as U
import Compiler.Utils.Graph.Base (Node (..))
import Data.Extensible (Lookup, type (>:))
import Data.Extensible.Effect (Eff, castEff)
import Data.Vector qualified as V
import GHC.Records (HasField (..))
import RIO
import RIO.List qualified as List (headMaybe, null, sortOn, uncons)
import RIO.Partial (fromJust)
import RIO.Set qualified as Set
import RIO.State (MonadState, State, execStateT, modify)

data CoalesceAllocation

instance R.RegisterAllocation CoalesceAllocation where
  allocateRegisters = allocateRegisters

allocateRegisters :: forall xs. Lookup xs "temp" U.UniqueEff => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProcedureX86 [Assembly Register])
allocateRegisters procedure = case coloring callerSaveRegisters procedure of
  Spilled spilled -> do
    procedure <- foldM startOver procedure spilled
    allocateRegisters procedure
  Colored allocation ->
    let body = getField @"val" <$> procedure.body
        allocatedBody = eliminateRedundantMove $ mapRegister allocation body
     in pure Procedure {body = allocatedBody, frame = procedure.frame}
    where
      mapRegister :: Allocation -> [Assembly U.Temp] -> [Assembly Register]
      mapRegister allocation = fmap (replaceRegister (fromJust . getColor allocation))
      eliminateRedundantMove :: [Assembly Register] -> [Assembly Register]
      eliminateRedundantMove = filter (\case MovRegister source target -> source /= target; _ -> True)

data ColoringResult = Spilled [U.Temp] | Colored Allocation

newtype SelectStack = SelectStack {stack :: [Set.Set U.Temp]}

newSelectStack :: SelectStack
newSelectStack = SelectStack []

push :: Set.Set U.Temp -> SelectStack -> SelectStack
push t stack = SelectStack $ t : stack.stack

pop :: SelectStack -> Maybe (Set.Set U.Temp, SelectStack)
pop stack = second SelectStack <$> List.uncons stack.stack

data StepResult = Executed | NotExecuted

isExecuted :: StepResult -> Bool
isExecuted Executed = True
isExecuted _ = False

coloring :: AvailableColors -> ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> ColoringResult
coloring colors procedure =
  let graph = buildInterfereceGraph (allTempRegisters `Set.union` Set.fromList (getAllocatedRegisters procedure.frame)) procedure.body
      stack =
        runST $ do
          mgraph <- Mutable.thaw graph
          flip execStateT newSelectStack . flip runReaderT colors $ simplifyCoalesceFreezeSpillLoop mgraph
   in select colors graph stack
  where
    simplifyCoalesceFreezeSpillLoop :: (PrimMonad m, MonadThrow m, MonadState SelectStack m, MonadReader AvailableColors m) => Mutable.InterferenceMutableGraph U.Temp (PrimState m) -> m ()
    simplifyCoalesceFreezeSpillLoop graph = do
      whenM (isExecuted <$> simplify graph) $ simplifyCoalesceFreezeSpillLoop graph
      whenM (isExecuted <$> coalesce graph) $ simplifyCoalesceFreezeSpillLoop graph
      whenM (isExecuted <$> freeze graph) $ simplifyCoalesceFreezeSpillLoop graph
      whenM (isExecuted <$> spill graph) $ simplifyCoalesceFreezeSpillLoop graph
      unlessM (Mutable.isEmpty graph) $ error "graph is expected to be empty here"

simplify :: (PrimMonad m, MonadThrow m, MonadState SelectStack m, MonadReader AvailableColors m) => Mutable.InterferenceMutableGraph U.Temp (PrimState m) -> m StepResult
simplify graph = do
  k <- asks length
  nodes <- getCandidatesOfSimplifying graph k
  if List.null nodes
    then pure NotExecuted
    else do
      forM_ nodes $ \node -> do
        modify $ push node.val.vars
        Mutable.removeNode graph node
      pure Executed
  where
    getCandidatesOfSimplifying :: (PrimMonad m, MonadThrow m, Ord var) => Mutable.InterferenceMutableGraph var (PrimState m) -> Int -> m [Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel]
    getCandidatesOfSimplifying mgraph k = List.sortOn (getField @"outDegree") . filter (\node -> not node.val.isMoveRelated) . filter (\node -> node.outDegree < k) . V.toList <$> Mutable.getAllNodes mgraph

coalesce :: (PrimMonad m, MonadThrow m, MonadState SelectStack m, MonadReader AvailableColors m) => Mutable.InterferenceMutableGraph U.Temp (PrimState m) -> m StepResult
coalesce graph = do
  coalesceableMoves <- Set.toList . foldMap (\node -> node.val.getCoalesceableMoves) <$> Mutable.getAllNodes graph
  candidates <- filterM (isBriggs graph `or` isGeorge graph) coalesceableMoves
  case List.headMaybe candidates of
    Nothing -> pure NotExecuted
    Just move -> do
      Mutable.coalesceMove graph move
      pure Executed
  where
    or :: (Monad m) => (a -> m Bool) -> (a -> m Bool) -> a -> m Bool
    or cond1 cond2 a = (||) <$> cond1 a <*> cond2 a

-- | http://www.cs.cmu.edu/afs/cs/academic/class/15745-s19/www/lectures/L23-Register-Coalescing.pdf
isBriggs :: (PrimMonad m, MonadThrow m, MonadReader AvailableColors m) => Mutable.InterferenceMutableGraph U.Temp (PrimState m) -> Move U.Temp -> m Bool
isBriggs graph move = do
  k <- asks length
  source <- Mutable.getNode graph move.source
  target <- Mutable.getNode graph move.destination
  let neiborhoods = V.uniq $ source.outIndexes V.++ target.outIndexes
  overDegreeNeiborhoods <- V.filter (\node -> node.outDegree >= k) <$> mapM (Mutable.getNodeByIndex graph) neiborhoods
  pure $ V.length overDegreeNeiborhoods < k

-- | http://www.cs.cmu.edu/afs/cs/academic/class/15745-s19/www/lectures/L23-Register-Coalescing.pdf
isGeorge :: (PrimMonad m, MonadThrow m, MonadReader AvailableColors m) => Mutable.InterferenceMutableGraph U.Temp (PrimState m) -> Move U.Temp -> m Bool
isGeorge graph move = do
  k <- asks length
  source <- Mutable.getNode graph move.source
  target <- Mutable.getNode graph move.destination
  let [lower, upper] = List.sortOn (getField @"outDegree") [source, target]
  neighborhoods <- mapM (Mutable.getNodeByIndex graph) lower.outIndexes
  pure $ all (\node -> V.elem upper.index node.outIndexes || node.outDegree < k) neighborhoods

freeze :: (PrimMonad m, MonadThrow m, MonadReader AvailableColors m) => Mutable.InterferenceMutableGraph U.Temp (PrimState m) -> m StepResult
freeze graph = do
  k <- asks length
  coalesceableMoves <- Set.toList . foldMap (\node -> node.val.getCoalesceableMoves) . V.filter (\node -> node.val.isMoveRelated && node.outDegree < k) <$> Mutable.getAllNodes graph
  case List.headMaybe coalesceableMoves of -- TODO: 選び方
    Nothing -> pure NotExecuted
    Just move -> do
      Mutable.freezeMove graph move
      pure Executed

spill :: (PrimMonad m, MonadThrow m, MonadState SelectStack m, MonadReader AvailableColors m) => Mutable.InterferenceMutableGraph U.Temp (PrimState m) -> m StepResult
spill graph = do
  k <- asks length
  candidates <- V.filter (\node -> node.outDegree >= k) <$> Mutable.getAllNodes graph
  if V.null candidates
    then pure NotExecuted
    else do
      let node = V.maximumBy (comparing (getField @"outDegree")) candidates -- TODO: 選び方
      modify $ push node.val.vars
      Mutable.removeNode graph node
      pure Executed

select :: AvailableColors -> Immutable.InterferenceGraph U.Temp -> SelectStack -> ColoringResult
select colors graph stack =
  let (missed, allocator) = runRegisterAllocatorState graph colors $ selectLoop stack
   in if List.null missed
        then Colored allocator.allocation
        else Spilled missed
  where
    selectLoop :: SelectStack -> State RegisterAllocator [U.Temp]
    selectLoop stack = case pop stack of
      Nothing -> pure []
      Just (top, remained) -> do
        allocated <- allocateEff top
        if allocated
          then selectLoop remained
          else (++) (Set.toList top) <$> selectLoop remained

startOver :: Lookup xs "temp" U.UniqueEff => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> U.Temp -> Eff xs (ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)])
startOver procedure spilledTemp = do
  (body, frame) <- castEff . flip runFrameEff procedure.frame $ do
    modifyFrameEff $ \frame -> frame {parameters = fmap (spillOutAccess spilledTemp) frame.parameters, localVariables = fmap (spillOutAccess spilledTemp) frame.localVariables}
    access <- allocateLocalEff True
    concat <$> mapM (insertSpillAssembly spilledTemp access) procedure.body :: Eff '["frame" >: FrameEff, "temp" >: U.UniqueEff] [L.ControlFlow U.Temp (Assembly U.Temp)]
  pure procedure {frame = frame, body = body}
  where
    spillOutAccess :: U.Temp -> Access -> Access
    spillOutAccess spilled (InRegister temp) | temp == spilled = SpilledOut
    spillOutAccess _ access = access
    insertSpillAssembly :: (Lookup xs "temp" U.UniqueEff, Lookup xs "frame" FrameEff) => U.Temp -> Access -> L.ControlFlow U.Temp (Assembly U.Temp) -> Eff xs [L.ControlFlow U.Temp (Assembly U.Temp)]
    insertSpillAssembly spilledTemp (InFrame memory) flow = do
      temp <- allocateNonEscapedLocalEff
      let newSrc = fmap (\register -> if register == spilledTemp then temp else register) flow.sources
          newDst = fmap (\register -> if register == spilledTemp then temp else register) flow.destinations
          newVal = fmap (\register -> if register == spilledTemp then temp else register) flow.val
          newFlow = L.setSources newSrc . L.setDestinations newDst $ flow {L.val = newVal}
      pure $
        [L.Instruction {src = [], dst = [temp], val = MovLoadIndirect memory (F.fp @Frame) temp} | spilledTemp `elem` flow.sources]
          ++ [newFlow]
          ++ [L.Instruction {src = [temp], dst = [], val = MovStoreIndirect temp memory (F.fp @Frame)} | spilledTemp `elem` flow.destinations]
    insertSpillAssembly _ _ _ = undefined
