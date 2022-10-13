module Compiler.Backend.X86.LivenessSpec (spec) where

import Compiler.Backend.X86.Arch (Label (Label'))
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (..), ControlFlows (ControlFlows), InterferenceGraph, InterferenceGraphEdgeLabel (..), buildInterfereceGraph, freeze)
import Compiler.Utils.Graph.Base
import Compiler.Utils.Graph.Mutable as Mutable
import RIO (Int, undefined, ($), (=<<))
import RIO.Set qualified as Set
import Test.Hspec (Spec, describe, it, pending, shouldBe)

spec :: Spec
spec = do
  buildInterfereceGraphSpec

buildInterfereceGraphSpec :: Spec
buildInterfereceGraphSpec = describe "interference graph" $ do
  it "empty: InterferenceGraph of empty code block's is empty graph" $ do
    let graph :: L.InterferenceGraph Int =
          L.buildInterfereceGraph $
            L.ControlFlows
              [ L.Label (Label' "entry") undefined,
                L.Label (Label' "done") undefined
              ]
              Set.empty
    expectedGraph :: L.InterferenceGraph Int <- L.freeze =<< Mutable.empty
    graph `shouldBe` expectedGraph

  it "An alive variable has an edge: InterferenceGraph of t1 <- 1; t2 <- 2; t1 <- t1 + t2 is t1 - t2" $ do
    let graph =
          L.buildInterfereceGraph $
            L.ControlFlows
              [ L.Label (Label' "entry") undefined,
                L.Instruction {src = [], dst = [1], val = undefined},
                L.Instruction {src = [], dst = [2], val = undefined},
                L.Instruction {src = [1, 2], dst = [1], val = undefined},
                L.Label (Label' "done") undefined
              ]
              (Set.fromList [1, 2])
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      Mutable.addEdgeByIndex graph node1.index node2.index L.InterferenceGraphEdgeLabel
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "An alive variable become unalive: t1 <- 1; t2 <- 2; t2 <- t1; t3 <- 3; t1 <- t1 + t3 is t1 - t2 t1 - t3" $ do
    let graph =
          L.buildInterfereceGraph $
            L.ControlFlows
              [ L.Label (Label' "entry") undefined,
                L.Instruction {src = [], dst = [1], val = undefined},
                L.Instruction {src = [], dst = [2], val = undefined},
                L.Instruction {src = [1, 2], dst = [1], val = undefined},
                L.Instruction {src = [], dst = [3], val = undefined},
                L.Instruction {src = [1, 3], dst = [3], val = undefined},
                L.Label (Label' "done") undefined
              ]
              (Set.fromList [1, 2, 3])
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      node3 <- Mutable.addNode graph 3
      Mutable.addEdgeByIndex graph node1.index node2.index L.InterferenceGraphEdgeLabel
      Mutable.addEdgeByIndex graph node1.index node3.index L.InterferenceGraphEdgeLabel
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "The defined variable interferes the variables used after the variable is defined: t1 <- 1; t2 <- 2; t3 <- 3; t2 <- t1 + t2 is t1 - t2 t1 - t3 t2 - t3" $ do
    let graph =
          L.buildInterfereceGraph $
            L.ControlFlows
              [ L.Label (Label' "entry") undefined,
                L.Instruction {src = [], dst = [1], val = undefined},
                L.Instruction {src = [], dst = [2], val = undefined},
                L.Instruction {src = [], dst = [3], val = undefined},
                L.Instruction {src = [1, 2], dst = [1], val = undefined},
                L.Label (Label' "done") undefined
              ]
              (Set.fromList [1, 2, 3])
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      node3 <- Mutable.addNode graph 3
      Mutable.addEdgeByIndex graph node1.index node2.index L.InterferenceGraphEdgeLabel
      Mutable.addEdgeByIndex graph node1.index node3.index L.InterferenceGraphEdgeLabel
      Mutable.addEdgeByIndex graph node2.index node3.index L.InterferenceGraphEdgeLabel
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "Not alive variable exist: t1 <- 1; t2 <- 2; t2 <- t1 + t2; t3 <- 3 is t1 - t2 t3" $ do
    let graph =
          L.buildInterfereceGraph $
            L.ControlFlows
              [ L.Label (Label' "entry") undefined,
                L.Instruction {src = [], dst = [1], val = undefined},
                L.Instruction {src = [], dst = [2], val = undefined},
                L.Instruction {src = [1, 2], dst = [1], val = undefined},
                L.Instruction {src = [], dst = [3], val = undefined},
                L.Label (Label' "done") undefined
              ]
              (Set.fromList [1, 2, 3])
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      node3 <- Mutable.addNode graph 3
      Mutable.addEdgeByIndex graph node1.index node2.index L.InterferenceGraphEdgeLabel
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "move doesn't interfere: t1 <- 1; t2 <- 2; t1 <- t2 is t1 t2" $ do
    let graph =
          L.buildInterfereceGraph $
            L.ControlFlows
              [ L.Label (Label' "entry") undefined,
                L.Instruction {src = [], dst = [1], val = undefined},
                L.Instruction {src = [], dst = [2], val = undefined},
                L.Move {src = [2], dst = [1], val = undefined},
                L.Label (Label' "done") undefined
              ]
              (Set.fromList [1, 2])
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "Jump: t1 <- 1; t1 <- t1 + 1; t2 <- 1; t2 <- t2 + 1; jump; t3 <- t1 + 1; t3 <- t3 + 1; label is t1 t2" $ do
    let graph =
          L.buildInterfereceGraph $
            L.ControlFlows
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
              (Set.fromList [1, 2, 3])
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      node3 <- Mutable.addNode graph 3
      L.freeze graph
    graph `shouldBe` expectedGraph

  it "CJump: t1 <- 1; t1 <- t1 + 1; t2 <- 1; t2 <- t2 + 1; cjump; t3 <- t1 + 1; t3 <- t3 + 1; label is t1 t2" $ do
    let graph =
          L.buildInterfereceGraph $
            L.ControlFlows
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
              (Set.fromList [1, 2, 3])
    expectedGraph <- do
      graph <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      node3 <- Mutable.addNode graph 3
      Mutable.addEdgeByIndex graph node1.index node2.index L.InterferenceGraphEdgeLabel
      L.freeze graph
    graph `shouldBe` expectedGraph
