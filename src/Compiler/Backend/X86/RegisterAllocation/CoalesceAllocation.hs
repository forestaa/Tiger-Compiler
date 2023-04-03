module Compiler.Backend.X86.RegisterAllocation.CoalesceAllocation (CoalesceAllocation) where

import Compiler.Backend.X86.Arch (Assembly (..), Register (..), callerSaveRegisters, replaceRegister)
import Compiler.Backend.X86.Frame (Access (..), Frame (..), FrameEff, ProcedureX86 (..), allTempRegisters, allocateLocalEff, allocateNonEscapedLocalEff, getAllocatedRegisters, modifyFrameEff, registerTempMap, runFrameEff)
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
import Data.Extensible.Effect (Eff, ReaderEff, State, askEff, asksEff, castEff, execStateEff, leaveEff, modifyEff, runReaderEff)
import Data.Foldable (maximumBy)
import Data.Vector qualified as V
import GHC.Records (HasField (..))
import RIO
import RIO.List qualified as List (null, sortOn, uncons)
import RIO.List.Partial as List (head)
import RIO.Partial (fromJust)
import RIO.Set qualified as Set
import RIO.State qualified as S (State)

data CoalesceAllocation

instance R.RegisterAllocation CoalesceAllocation where
  allocateRegisters = allocateRegisters

allocateRegisters :: forall xs. Lookup xs "temp" U.UniqueEff => ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProcedureX86 [Assembly Register])
allocateRegisters procedure = case coloring callerSaveRegisters procedure of
  Spilled spilled -> do
    procedure <- startOver procedure spilled
    allocateRegisters procedure
  Colored allocation ->
    let body = (.val) <$> procedure.body
        allocatedBody = eliminateRedundantMove $ mapRegister allocation body
     in pure Procedure {body = allocatedBody, frame = procedure.frame}
    where
      mapRegister :: Allocation -> [Assembly U.Temp] -> [Assembly Register]
      mapRegister allocation = fmap (replaceRegister (fromJust . getColor allocation))
      eliminateRedundantMove :: [Assembly Register] -> [Assembly Register]
      eliminateRedundantMove = filter (\case MovRegister source target -> source /= target; _ -> True)

data ColoringResult = Spilled U.Temp | Colored Allocation

newtype SelectStack = SelectStack {stack :: [Set.Set U.Temp]} deriving (Show)

newSelectStack :: SelectStack
newSelectStack = SelectStack []

push :: Set.Set U.Temp -> SelectStack -> SelectStack
push t stack = SelectStack $ t : stack.stack

pop :: SelectStack -> Maybe (Set.Set U.Temp, SelectStack)
pop stack = second SelectStack <$> List.uncons stack.stack

coloring :: AvailableColors -> ProcedureX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> ColoringResult
coloring colors procedure =
  let graph = buildInterfereceGraph (Set.fromList (registerTempMap <$> colors) `Set.union` Set.fromList (getAllocatedRegisters procedure.frame)) procedure.body
      stack = leaveEff . flip (execStateEff @"select") newSelectStack . flip (runReaderEff @"precolored") (Set.fromList allTempRegisters) . flip (runReaderEff @"colors") colors $ simplifyCoalesceFreezeSpillLoop graph
   in select colors graph stack
  where
    simplifyCoalesceFreezeSpillLoop :: (Lookup xs "select" (State SelectStack), Lookup xs "precolored" (ReaderEff (Set.Set U.Temp)), Lookup xs "colors" (ReaderEff AvailableColors)) => Immutable.InterferenceGraph U.Temp -> Eff xs ()
    simplifyCoalesceFreezeSpillLoop graph = do
      nextStep <- determineNextStep graph
      case nextStep of
        Simplify candidates -> simplify graph candidates >>= simplifyCoalesceFreezeSpillLoop
        Coalesce candidates -> coalesce graph candidates >>= simplifyCoalesceFreezeSpillLoop
        Freeze candidates -> freeze graph candidates >>= simplifyCoalesceFreezeSpillLoop
        Spill candidates -> spill graph candidates >>= simplifyCoalesceFreezeSpillLoop
        Finished ->
          forM_ (Immutable.getAllNodes graph) $ \node -> do
            modifyEff #select $ push node.val.vars

data NextStep var
  = Simplify (Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel))
  | Coalesce [Move var]
  | Freeze [Move var]
  | Spill (Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel))
  | Finished

determineNextStep :: (Lookup xs "colors" (ReaderEff AvailableColors), Lookup xs "precolored" (ReaderEff (Set.Set U.Temp))) => Immutable.InterferenceGraph U.Temp -> Eff xs (NextStep U.Temp)
determineNextStep graph = do
  simpifyCandidates <- getCandidatesOfSimplify graph
  coalesceCandidates <- getCandidatesOfCoalesce graph
  freezeCandidates <- getCandidatesOfFreeze graph
  spillCandidates <- getCandidatesOfSpill graph
  notColored <- getNotPrecoloredNode graph
  if
      | not (null simpifyCandidates) -> pure $ Simplify simpifyCandidates
      | not (null coalesceCandidates) -> pure $ Coalesce coalesceCandidates
      | not (null freezeCandidates) -> pure $ Freeze freezeCandidates
      | not (null spillCandidates) -> pure $ Spill spillCandidates
      | null notColored -> pure Finished
      | otherwise -> error "graph is expected to be precolored here"
  where
    getNotPrecoloredNode :: Lookup xs "precolored" (ReaderEff (Set.Set U.Temp)) => Immutable.InterferenceGraph U.Temp -> Eff xs (Vector (Node (InterferenceGraphNode U.Temp) InterferenceGraphEdgeLabel))
    getNotPrecoloredNode graph = V.filterM isNotPrecoloredNode $ Immutable.getAllNodes graph

isNotPrecoloredNode :: (Lookup xs "precolored" (ReaderEff (Set.Set var)), Ord var) => Node (InterferenceGraphNode var) label -> Eff xs Bool
isNotPrecoloredNode node = asksEff #precolored $ Set.null . Set.intersection node.val.vars

isNotPrecoloredMove :: (Lookup xs "precolored" (ReaderEff (Set.Set var)), Ord var) => Move var -> Eff xs Bool
isNotPrecoloredMove move = do
  precolored <- askEff #precolored
  pure $ move.source `Set.notMember` precolored || move.destination `Set.notMember` precolored

getCandidatesOfSimplify :: (Lookup xs "colors" (ReaderEff AvailableColors), Lookup xs "precolored" (ReaderEff (Set.Set var)), Ord var) => Immutable.InterferenceGraph var -> Eff xs (Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel))
getCandidatesOfSimplify graph = do
  k <- asksEff #colors length
  V.filterM isNotPrecoloredNode . V.filter (\node -> node.outDegree < k && not node.val.isMoveRelated) $ Immutable.getAllNodes graph

simplify :: (Lookup xs "select" (State SelectStack), Lookup xs "colors" (ReaderEff AvailableColors)) => Immutable.InterferenceGraph U.Temp -> Vector (Node (InterferenceGraphNode U.Temp) InterferenceGraphEdgeLabel) -> Eff xs (Immutable.InterferenceGraph U.Temp)
simplify graph candidates = do
  let node = maximumBy (comparing (.outDegree)) candidates
  modifyEff #select $ push node.val.vars
  pure $ runST $ do
    mgraph <- Mutable.thaw graph
    Mutable.removeNode mgraph node
    Mutable.freeze mgraph

getCandidatesOfCoalesce :: (Lookup xs "colors" (ReaderEff AvailableColors), Lookup xs "precolored" (ReaderEff (Set.Set var)), Ord var) => Immutable.InterferenceGraph var -> Eff xs [Move var]
getCandidatesOfCoalesce graph = do
  coalesceableMoves <- filterM isNotPrecoloredMove . Set.toList . foldMap (.val.getCoalesceableMoves) $ Immutable.getAllNodes graph
  filterM (isBriggs graph `or` isGeorge graph) coalesceableMoves
  where
    or :: (Monad m) => (a -> m Bool) -> (a -> m Bool) -> a -> m Bool
    or cond1 cond2 a = (||) <$> cond1 a <*> cond2 a

-- | http://www.cs.cmu.edu/afs/cs/academic/class/15745-s19/www/lectures/L23-Register-Coalescing.pdf
isBriggs :: (Lookup xs "colors" (ReaderEff AvailableColors), Ord var) => Immutable.InterferenceGraph var -> Move var -> Eff xs Bool
isBriggs graph move = do
  k <- asksEff #colors length
  let source = Immutable.getNode graph move.source
      target = Immutable.getNode graph move.destination
      neiborhoods = V.uniq $ source.outIndexes V.++ target.outIndexes
      overDegreeNeiborhoods = V.filter (\node -> node.outDegree >= k) $ V.map (Immutable.getNodeByIndex graph) neiborhoods
  pure $ V.length overDegreeNeiborhoods < k

-- | http://www.cs.cmu.edu/afs/cs/academic/class/15745-s19/www/lectures/L23-Register-Coalescing.pdf
isGeorge :: (Lookup xs "colors" (ReaderEff AvailableColors), Ord var) => Immutable.InterferenceGraph var -> Move var -> Eff xs Bool
isGeorge graph move = do
  k <- asksEff #colors length
  let source = Immutable.getNode graph move.source
      target = Immutable.getNode graph move.destination
      [lower, upper] = List.sortOn (.outDegree) [source, target]
      neighborhoods = Immutable.getNodeByIndex graph <$> lower.outIndexes
  pure $ all (\node -> V.elem upper.index node.outIndexes || node.outDegree < k) neighborhoods

coalesce :: (Lookup xs "select" (State SelectStack), Lookup xs "colors" (ReaderEff AvailableColors)) => Immutable.InterferenceGraph U.Temp -> [Move U.Temp] -> Eff xs (Immutable.InterferenceGraph U.Temp)
coalesce graph candidates = pure $ runST $ do
  let move = List.head candidates
  mgraph <- Mutable.thaw graph
  Mutable.coalesceMove mgraph move
  Mutable.freeze mgraph

getCandidatesOfFreeze :: (Lookup xs "colors" (ReaderEff AvailableColors), Lookup xs "precolored" (ReaderEff (Set.Set var)), Ord var) => Immutable.InterferenceGraph var -> Eff xs [Move var]
getCandidatesOfFreeze graph = do
  k <- asksEff #colors length
  filterM isNotPrecoloredMove . Set.toList . foldMap (\node -> node.val.getCoalesceableMoves) . V.filter (\node -> node.val.isMoveRelated && node.outDegree < k) $ Immutable.getAllNodes graph

freeze :: (Lookup xs "select" (State SelectStack), Lookup xs "colors" (ReaderEff AvailableColors)) => Immutable.InterferenceGraph U.Temp -> [Move U.Temp] -> Eff xs (Immutable.InterferenceGraph U.Temp)
freeze graph candidates = pure $ runST $ do
  let move = List.head candidates
  mgraph <- Mutable.thaw graph
  Mutable.freezeMove mgraph move
  Mutable.freeze mgraph

getCandidatesOfSpill :: (Lookup xs "colors" (ReaderEff AvailableColors), Lookup xs "precolored" (ReaderEff (Set.Set var)), Ord var) => Immutable.InterferenceGraph var -> Eff xs (Vector (Node (InterferenceGraphNode var) InterferenceGraphEdgeLabel))
getCandidatesOfSpill graph = do
  k <- asksEff #colors length
  V.filterM isNotPrecoloredNode . V.filter (\node -> node.outDegree >= k) $ Immutable.getAllNodes graph

spill :: (Lookup xs "select" (State SelectStack), Lookup xs "colors" (ReaderEff AvailableColors)) => Immutable.InterferenceGraph U.Temp -> Vector (Node (InterferenceGraphNode U.Temp) InterferenceGraphEdgeLabel) -> Eff xs (Immutable.InterferenceGraph U.Temp)
spill graph candidates = do
  let node = V.maximumBy (comparing (.outDegree)) candidates -- TODO: select by rank, which is computed by count to be used the variable, or something
  modifyEff #select $ push node.val.vars
  pure $ runST $ do
    mgraph <- Mutable.thaw graph
    Mutable.removeNode mgraph node
    Mutable.freeze mgraph

select :: AvailableColors -> Immutable.InterferenceGraph U.Temp -> SelectStack -> ColoringResult
select colors graph stack =
  let (missed, allocator) = runRegisterAllocatorState graph colors $ selectLoop stack
   in if List.null missed
        then Colored allocator.allocation
        else
          let spilled = maximumBy (comparing (\node -> (Immutable.getNode graph node).outDegree)) missed
           in Spilled spilled
  where
    selectLoop :: SelectStack -> S.State RegisterAllocator [U.Temp]
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
    insertSpillAssembly spilledTemp (InFrame memory) flow
      | spilledTemp `elem` flow.sources
          || spilledTemp `elem` flow.destinations = do
          temp <- allocateNonEscapedLocalEff
          let newSrc = fmap (\register -> if register == spilledTemp then temp else register) flow.sources
              newDst = fmap (\register -> if register == spilledTemp then temp else register) flow.destinations
              newVal = fmap (\register -> if register == spilledTemp then temp else register) flow.val
              newFlow = L.setSources newSrc . L.setDestinations newDst $ flow {L.val = newVal}
          pure $
            [L.Instruction {src = [], dst = [temp], val = MovLoadIndirect memory (F.fp @Frame) temp} | spilledTemp `elem` flow.sources]
              ++ [newFlow]
              ++ [L.Instruction {src = [temp], dst = [], val = MovStoreIndirect temp memory (F.fp @Frame)} | spilledTemp `elem` flow.destinations]
      | otherwise = pure [flow]
    insertSpillAssembly _ _ _ = undefined
