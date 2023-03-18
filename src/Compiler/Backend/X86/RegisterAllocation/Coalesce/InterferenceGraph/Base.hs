module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base
  ( InterferenceGraphEdgeLabel (..),
    InterferenceGraphNode (vars),
    Move (source, destination),
    newInterferenceGraphNode,
    addMove,
    coalesceMove,
    freezeMove,
    constrainMove,
    newMove,
  )
where

import GHC.Records (HasField (..))
import RIO
import RIO.Set qualified as Set (Set, filter, fromList, insert, singleton, union)

data InterferenceGraphNode var = InterferenceGraphNode {vars :: Set.Set var, moves :: Set (Move var)} deriving (Show)

instance HasField "isMoveRelated" (InterferenceGraphNode var) Bool where
  getField node = any (getField @"isCoalesceable") node.moves

instance HasField "getCoalesceableMoves" (InterferenceGraphNode var) (Set (Move var)) where
  getField node = Set.filter (getField @"isCoalesceable") node.moves

newInterferenceGraphNode :: Ord var => var -> [Move var] -> InterferenceGraphNode var
newInterferenceGraphNode var moves = InterferenceGraphNode {vars = Set.singleton var, moves = Set.fromList moves}

addMove :: Ord var => Move var -> InterferenceGraphNode var -> InterferenceGraphNode var
addMove move node = node {moves = Set.insert move node.moves}

coalesceMove :: Ord var => InterferenceGraphNode var -> InterferenceGraphNode var -> Move var -> InterferenceGraphNode var
coalesceMove node1 node2 move = InterferenceGraphNode {vars = node1.vars `Set.union` node2.vars, moves = Set.insert (coalesce move) $ node1.moves `Set.union` node2.moves}

freezeMove :: Ord var => Move var -> InterferenceGraphNode var -> InterferenceGraphNode var
freezeMove move node = node {moves = Set.insert (freeze move) node.moves}

constrainMove :: Ord var => Move var -> InterferenceGraphNode var -> InterferenceGraphNode var
constrainMove move node = node {moves = Set.insert (constrain move) node.moves}

data Move var = Move {source :: var, destination :: var, status :: MoveStatus} deriving (Show)

instance Eq var => Eq (Move var) where
  move1 == move2 = (move1.source, move1.destination) == (move2.source, move2.destination)

instance Ord var => Ord (Move var) where
  move1 `compare` move2 = (move1.source, move1.destination) `compare` (move2.source, move2.destination)

-- MEMO: ActiveはGraphとともに判断するので不要
data MoveStatus
  = Coalesceable
  | Coalesced
  | -- | Give up coalescing
    Frozen
  | -- | source and destination are interfered
    Constrained
  deriving (Show, Eq)

instance HasField "isFrozen" (Move var) Bool where
  getField move = move.status == Frozen

instance HasField "isCoalesceable" (Move var) Bool where
  getField move = move.status == Coalesceable

newMove :: var -> var -> Move var
newMove source destination = Move {source, destination, status = Coalesceable}

coalesce :: Move var -> Move var
coalesce move = move {status = Coalesced}

freeze :: Move var -> Move var
freeze move = move {status = Frozen}

constrain :: Move var -> Move var
constrain move = move {status = Constrained}

data InterferenceGraphEdgeLabel = InterferenceGraphEdgeLabel deriving (Show, Eq, Ord)
