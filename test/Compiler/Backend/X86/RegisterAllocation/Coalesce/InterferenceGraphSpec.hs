module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraphSpec (spec) where

import Compiler.Backend.X86.Arch (Label (Label'))
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), ControlFlowGraph, ControlFlowNode (..), freeze, newControlFlowNode)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph (buildInterfereceGraph, newInterferenceGraph)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base (newMove)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base qualified as B
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Immutable qualified as C
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable qualified as C
import Compiler.Utils.Graph.Base (Edge (..), Node (..), NodeIndex (..))
import Compiler.Utils.Graph.Mutable as Mutable
import RIO (Int, undefined, ($), (=<<))
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  newInterferenceGraphSpec
  buildInterfereceGraphSpec

newInterferenceGraphSpec :: Spec
newInterferenceGraphSpec = describe "newInterferenceGraphSpec" $ do
  it "add edges (a,b_1) ... (a,b_j) for any instruction which defines a and is not move where b_1..b_j are live-out and are not a" $ do
    let cnode = (L.newControlFlowNode (L.Instruction {src = [], dst = [1], val = 1}) 1) {L.liveInVariables = Set.empty, L.liveOutVariables = Set.fromList [1, 2, 3]}
        vars = Set.fromList [1, 2, 3]
    cfGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode
      L.freeze graph
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      _ <- C.addEdgeByIndex graph node1.index node2.index
      _ <- C.addEdgeByIndex graph node1.index node3.index
      C.freeze graph
    let graph = newInterferenceGraph vars cfGraph
    graph `shouldBe` expectedGraph

  it "do not add edges (a,b_1) ... (a,b_j) for any instruction which does not define a and is not move where b_1..b_j are live-out and are not a" $ do
    let cnode = (L.newControlFlowNode (L.Instruction {src = [], dst = [], val = 1}) 1) {L.liveInVariables = Set.empty, L.liveOutVariables = Set.fromList [1, 2, 3]}
        vars = Set.fromList [1, 2, 3]
    cfGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode
      L.freeze graph
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      C.freeze graph
    let graph = newInterferenceGraph vars cfGraph
    graph `shouldBe` expectedGraph

  it "add edges (a,b_1) ... (a,b_j) and add move for move instruction a <- c where b_1..b_j are live-out and are not a and c" $ do
    let cnode = (L.newControlFlowNode (L.Move {src = [2], dst = [1], val = 1}) 1) {L.liveInVariables = Set.empty, L.liveOutVariables = Set.fromList [1, 2, 3]}
        vars = Set.fromList [1, 2, 3]
    cfGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode
      L.freeze graph
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      _ <- C.addEdgeByIndex graph node1.index node3.index
      C.addMove graph $ newMove 2 1
      C.freeze graph
    let graph = newInterferenceGraph vars cfGraph
    graph `shouldBe` expectedGraph

  it "constrain move between interfered node" $ do
    let cnode1 = (L.newControlFlowNode (L.Instruction {src = [2], dst = [1], val = 1}) 1) {L.liveInVariables = Set.empty, L.liveOutVariables = Set.fromList [3]}
        cnode2 = (L.newControlFlowNode (L.Move {src = [3], dst = [1], val = 1}) 2) {L.liveInVariables = Set.empty, L.liveOutVariables = Set.empty}
        vars = Set.fromList [1, 2, 3]
    cfGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph cnode1
      node2 <- Mutable.addNode graph cnode2
      L.freeze graph
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      _ <- C.addEdgeByIndex graph node1.index node3.index
      C.constrainMove graph $ newMove 3 1
      C.freeze graph
    let graph = newInterferenceGraph vars cfGraph
    graph `shouldBe` expectedGraph

buildInterfereceGraphSpec :: Spec
buildInterfereceGraphSpec = describe "buildInterferenceGraph" $ do
  it "empty: InterferenceGraph of empty code block's is empty graph" $ do
    let graph :: C.InterferenceGraph Int =
          buildInterfereceGraph
            Set.empty
            [ L.Label (Label' "entry") undefined,
              L.Label (Label' "done") undefined
            ]
    expectedGraph :: C.InterferenceGraph Int <- C.freeze =<< C.empty
    graph `shouldBe` expectedGraph

  it "An alive variable has an edge: InterferenceGraph of t1 <- 1; t2 <- 2; t1 <- t1 + t2 is t1 - t2" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Instruction {src = [1, 2], dst = [1], val = undefined},
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      C.addEdgeByIndex graph node1.index node2.index
      C.freeze graph
    graph `shouldBe` expectedGraph

  it "An alive variable become unalive: t1 <- 1; t2 <- 2; t2 <- t1; t3 <- 3; t1 <- t1 + t3 is t1 - t2 t1 - t3" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2, 3])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Instruction {src = [1, 2], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [3], val = undefined},
              L.Instruction {src = [1, 3], dst = [3], val = undefined},
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      C.addEdgeByIndex graph node1.index node2.index
      C.addEdgeByIndex graph node1.index node3.index
      C.freeze graph
    graph `shouldBe` expectedGraph

  it "The defined variable interferes the variables used after the variable is defined: t1 <- 1; t2 <- 2; t3 <- 3; t2 <- t1 + t2 is t1 - t2 t1 - t3 t2 - t3" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2, 3])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Instruction {src = [], dst = [3], val = undefined},
              L.Instruction {src = [1, 2], dst = [1], val = undefined},
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      C.addEdgeByIndex graph node1.index node2.index
      C.addEdgeByIndex graph node1.index node3.index
      C.addEdgeByIndex graph node2.index node3.index
      C.freeze graph
    graph `shouldBe` expectedGraph

  it "Not alive variable exist: t1 <- 1; t2 <- 2; t2 <- t1 + t2; t3 <- 3 is t1 - t2 t3" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2, 3])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Instruction {src = [1, 2], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [3], val = undefined},
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      C.addEdgeByIndex graph node1.index node2.index
      C.freeze graph
    graph `shouldBe` expectedGraph

  it "move doesn't interfere: t1 <- 1; t2 <- 2; t1 <- t2 is t1 t2" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Move {src = [2], dst = [1], val = undefined},
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      C.addMove graph $ newMove 2 1
      C.freeze graph
    graph `shouldBe` expectedGraph

  it "move is constrained between interfered node: t1 <- 1; t2 <- 2; t1 <- t1 + t2; t2 <- t1 is t1 - t2" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Instruction {src = [1, 2], dst = [1], val = undefined},
              L.Move {src = [1], dst = [2], val = undefined},
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      C.addEdgeByIndex graph node1.index node2.index
      C.constrainMove graph $ newMove 1 2
      C.freeze graph
    graph `shouldBe` expectedGraph

  it "Jump: t1 <- 1; t1 <- t1 + 1; t2 <- 1; t2 <- t2 + 1; jump; t3 <- t1 + 1; t3 <- t3 + 1; label is t1 t2" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2, 3])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [1], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Instruction {src = [2], dst = [2], val = undefined},
              L.Jump {jumps = [Label' "label"], val = undefined},
              L.Instruction {src = [1], dst = [3], val = undefined},
              L.Instruction {src = [3], dst = [3], val = undefined},
              L.Label (Label' "label") undefined,
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      C.freeze graph
    graph `shouldBe` expectedGraph

  it "CJump: t1 <- 1; t1 <- t1 + 1; t2 <- 1; t2 <- t2 + 1; cjump; t3 <- t1 + 1; t3 <- t3 + 1; label is t1 t2" $ do
    let graph =
          buildInterfereceGraph
            (Set.fromList [1, 2, 3])
            [ L.Label (Label' "entry") undefined,
              L.Instruction {src = [], dst = [1], val = undefined},
              L.Instruction {src = [1], dst = [1], val = undefined},
              L.Instruction {src = [], dst = [2], val = undefined},
              L.Instruction {src = [2], dst = [2], val = undefined},
              L.CJump {jumps = [Label' "label"], val = undefined},
              L.Instruction {src = [1], dst = [3], val = undefined},
              L.Instruction {src = [3], dst = [3], val = undefined},
              L.Label (Label' "label") undefined,
              L.Label (Label' "done") undefined
            ]
    expectedGraph <- do
      graph <- C.empty
      node1 <- C.addNode graph 1
      node2 <- C.addNode graph 2
      node3 <- C.addNode graph 3
      C.addEdgeByIndex graph node1.index node2.index
      C.freeze graph
    graph `shouldBe` expectedGraph
