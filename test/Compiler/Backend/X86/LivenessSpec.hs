module Compiler.Backend.X86.LivenessSpec (spec) where

import Compiler.Backend.X86.Arch (Label (Label'))
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), ControlFlowGraph, LiveVariables (..), freeze, newControlFlowGraph, newControlFlowNode, solveDataFlowEquation)
import Compiler.Utils.Graph.Base (Node (index))
import Compiler.Utils.Graph.Mutable as Mutable (MutableGraph (addEdgeByIndex, addNode, empty))
import RIO (Int, ($))
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Test.Hspec (Spec, describe, it, pending, shouldBe)

spec :: Spec
spec = do
  newControlFlowGraphSpec
  solveDataFlowEquationSpec

newControlFlowGraphSpec :: Spec
newControlFlowGraphSpec = describe "newControlFlow spec" $ do
  it "A usual instruction has a successor" $ do
    let graph :: L.ControlFlowGraph Int Int =
          L.newControlFlowGraph
            [ L.Label (Label' "entry") 0,
              L.Instruction {src = [], dst = [], val = 1},
              L.Label (Label' "done") 2
            ]
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph $ L.newControlFlowNode (L.Label (Label' "entry") 1) 0
      node2 <- Mutable.addNode graph $ L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 0}) 1
      node3 <- Mutable.addNode graph $ L.newControlFlowNode (L.Label (Label' "done") 2) 2
      _ <- Mutable.addEdgeByIndex graph node1.index node2.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node3.index ()
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "jump does not go next but jump to label" $ do
    let graph :: L.ControlFlowGraph Int Int =
          L.newControlFlowGraph
            [ L.Label (Label' "entry") 0,
              L.Instruction {src = [], dst = [], val = 1},
              L.Jump {jumps = [Label' "done"], val = 2},
              L.Instruction {src = [], dst = [], val = 3},
              L.Label (Label' "done") 4
            ]
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph $ L.newControlFlowNode (L.Label (Label' "entry") 0) 0
      node2 <- Mutable.addNode graph $ L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 1}) 1
      node3 <- Mutable.addNode graph $ L.newControlFlowNode (L.Jump {jumps = [Label' "done"], val = 2}) 2
      node4 <- Mutable.addNode graph $ L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 3}) 3
      node5 <- Mutable.addNode graph $ L.newControlFlowNode (L.Label (Label' "done") 4) 4
      _ <- Mutable.addEdgeByIndex graph node1.index node2.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node3.index ()
      _ <- Mutable.addEdgeByIndex graph node3.index node5.index ()
      _ <- Mutable.addEdgeByIndex graph node4.index node5.index ()
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "cjump does go next and jump to label" $ do
    let graph :: L.ControlFlowGraph Int Int =
          L.newControlFlowGraph
            [ L.Label (Label' "entry") 0,
              L.Instruction {src = [], dst = [], val = 1},
              L.CJump {jumps = [Label' "done"], val = 2},
              L.Instruction {src = [], dst = [], val = 3},
              L.Label (Label' "done") 4
            ]
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph $ L.newControlFlowNode (L.Label (Label' "entry") 0) 0
      node2 <- Mutable.addNode graph $ L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 1}) 1
      node3 <- Mutable.addNode graph $ L.newControlFlowNode (L.Jump {jumps = [Label' "done"], val = 2}) 2
      node4 <- Mutable.addNode graph $ L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 3}) 3
      node5 <- Mutable.addNode graph $ L.newControlFlowNode (L.Label (Label' "done") 4) 4
      _ <- Mutable.addEdgeByIndex graph node1.index node2.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node3.index ()
      _ <- Mutable.addEdgeByIndex graph node3.index node4.index ()
      _ <- Mutable.addEdgeByIndex graph node3.index node5.index ()
      _ <- Mutable.addEdgeByIndex graph node4.index node5.index ()
      L.freeze graph
    graph `shouldBe` expectedGraph

solveDataFlowEquationSpec :: Spec
solveDataFlowEquationSpec = describe "solveDataFlowEquation spec" $ do
  it "A variable is live between defined and used" $ do
    let cnode1 = L.newControlFlowNode (L.Instruction {src = [], dst = [1], val = 1}) 1
        cnode2 = L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 2}) 2
        cnode3 = L.newControlFlowNode (L.Instruction {src = [1], dst = [], val = 3}) 3
    graph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode1
      node2 <- Mutable.addNode graph cnode2
      node3 <- Mutable.addNode graph cnode3
      _ <- Mutable.addEdgeByIndex graph node1.index node2.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node3.index ()
      L.freeze graph
    let liveVariablesMap = L.solveDataFlowEquation graph
        expectedLiveVariablesMap =
          Map.fromList
            [ (cnode1, L.LiveVariable cnode1 Set.empty (Set.fromList [1])),
              (cnode2, L.LiveVariable cnode2 (Set.fromList [1]) (Set.fromList [1])),
              (cnode3, L.LiveVariable cnode3 (Set.fromList [1]) Set.empty)
            ]
    liveVariablesMap `shouldBe` expectedLiveVariablesMap

  it "A variable is dead after used" $ do
    let cnode1 = L.newControlFlowNode (L.Instruction {src = [], dst = [1], val = 1}) 1
        cnode2 = L.newControlFlowNode (L.Instruction {src = [1], dst = [], val = 2}) 2
        cnode3 = L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 3}) 3
    graph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode1
      node2 <- Mutable.addNode graph cnode2
      node3 <- Mutable.addNode graph cnode3
      _ <- Mutable.addEdgeByIndex graph node1.index node2.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node3.index ()
      L.freeze graph
    let liveVariablesMap = L.solveDataFlowEquation graph
        expectedLiveVariablesMap =
          Map.fromList
            [ (cnode1, L.LiveVariable cnode1 Set.empty (Set.fromList [1])),
              (cnode2, L.LiveVariable cnode2 (Set.fromList [1]) Set.empty),
              (cnode3, L.LiveVariable cnode3 Set.empty Set.empty)
            ]
    liveVariablesMap `shouldBe` expectedLiveVariablesMap

  it "A variable is dead between doubly defined and not used" $ do
    let cnode1 = L.newControlFlowNode (L.Instruction {src = [], dst = [1], val = 1}) 1
        cnode2 = L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 2}) 2
        cnode3 = L.newControlFlowNode (L.Instruction {src = [], dst = [1], val = 3}) 3
        cnode4 = L.newControlFlowNode (L.Instruction {src = [1], dst = [], val = 4}) 4
    graph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode1
      node2 <- Mutable.addNode graph cnode2
      node3 <- Mutable.addNode graph cnode3
      node4 <- Mutable.addNode graph cnode4
      _ <- Mutable.addEdgeByIndex graph node1.index node2.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node3.index ()
      _ <- Mutable.addEdgeByIndex graph node3.index node4.index ()
      L.freeze graph
    let liveVariablesMap = L.solveDataFlowEquation graph
        expectedLiveVariablesMap =
          Map.fromList
            [ (cnode1, L.LiveVariable cnode1 Set.empty Set.empty),
              (cnode2, L.LiveVariable cnode2 Set.empty Set.empty),
              (cnode3, L.LiveVariable cnode3 Set.empty (Set.fromList [1])),
              (cnode4, L.LiveVariable cnode4 (Set.fromList [1]) Set.empty)
            ]
    liveVariablesMap `shouldBe` expectedLiveVariablesMap

  it "A variable is live-out if the variable is live on any output path" $ do
    let cnode1 = L.newControlFlowNode (L.Instruction {src = [], dst = [1], val = 1}) 1
        cnode2 = L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 2}) 2
        cnode3 = L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 3}) 3
        cnode4 = L.newControlFlowNode (L.Instruction {src = [1], dst = [], val = 4}) 4
    graph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode1
      node2 <- Mutable.addNode graph cnode2
      node3 <- Mutable.addNode graph cnode3
      node4 <- Mutable.addNode graph cnode4
      _ <- Mutable.addEdgeByIndex graph node1.index node2.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node3.index ()
      _ <- Mutable.addEdgeByIndex graph node2.index node4.index ()
      L.freeze graph
    let liveVariablesMap = L.solveDataFlowEquation graph
        expectedLiveVariablesMap =
          Map.fromList
            [ (cnode1, L.LiveVariable cnode1 Set.empty (Set.fromList [1])),
              (cnode2, L.LiveVariable cnode2 (Set.fromList [1]) (Set.fromList [1])),
              (cnode3, L.LiveVariable cnode3 Set.empty Set.empty),
              (cnode4, L.LiveVariable cnode4 (Set.fromList [1]) Set.empty)
            ]
    liveVariablesMap `shouldBe` expectedLiveVariablesMap
