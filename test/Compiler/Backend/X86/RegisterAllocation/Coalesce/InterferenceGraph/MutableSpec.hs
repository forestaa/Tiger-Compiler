module Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.MutableSpec (spec) where

import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Base qualified as B (InterferenceGraphEdgeLabel (..), InterferenceGraphNode (..), Move (destination, source), addMove, coalesceMove, constrainMove, freezeMove, newInterferenceGraphNode, newMove, removeNode)
import Compiler.Backend.X86.RegisterAllocation.Coalesce.InterferenceGraph.Mutable
import Compiler.Utils.Graph.Base
import Compiler.Utils.Maybe
import Data.Vector qualified as V
import GHC.Records (HasField (getField))
import RIO hiding (reverse)
import RIO.Set qualified as Set
import Test.Hspec (Spec, describe, expectationFailure, it, pending, shouldBe, shouldThrow)

spec :: Spec
spec = do
  interferenceGraphMutableSpec
  freezeThawSpec

interferenceGraphMutableSpec :: Spec
interferenceGraphMutableSpec = describe "InterferenceGraph Mutable spec" $ do
  it "empty -> getNode -> KeyNotFound Exception" $ do
    shouldThrow
      ( do
          graph <- empty
          getNode graph 1
      )
      (KeyNotFound ==)

  it "empty -> addNode -> getNode" $ do
    node <- do
      graph <- empty
      addNode graph 1
      getNode graph 1
    node.val.vars `shouldBe` Set.singleton 1

  it "empty -> addNode*2(same) -> KeyAlreadyExist Exception" $ do
    shouldThrow
      ( do
          graph <- empty
          addNode graph 1
          addNode graph 1
          pure ()
      )
      (KeyAlradyExist ==)

  it "empty -> addNode -> getNodeByIndex" $ do
    node <- do
      graph <- empty
      node <- addNode graph 1
      getNodeByIndex graph node.index
    node.val.vars `shouldBe` Set.singleton 1

  it "empty -> addNode*3 -> getAllNodes" $ do
    nodes <- do
      graph <- empty
      addNode graph 1
      addNode graph 2
      addNode graph 3
      getAllNodes graph
    V.length nodes `shouldBe` 3
    (nodes V.!? 0).val.vars `shouldBe` Just (Set.singleton 1)
    (nodes V.!? 1).val.vars `shouldBe` Just (Set.singleton 2)
    (nodes V.!? 2).val.vars `shouldBe` Just (Set.singleton 3)

  it "empty -> addEdge -> KeyNotFound Exception" $ do
    shouldThrow
      ( do
          graph <- empty
          addEdge graph 1 2
      )
      (KeyNotFound ==)

  it "empty -> addNode*3 -> addEdge -> getEdge" $ do
    (node1, node2, edges) <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      addEdge graph 1 2
      edges <- getEdgeByIndex graph node1.index node2.index
      pure (node1, node2, edges)
    fmap extractEdge edges
      `shouldBe` Just (node1.index, node2.index, B.InterferenceGraphEdgeLabel)

  it "empty -> addNode*3 -> addEdgeByIndex*3 -> node.outEdges" $ do
    (node1, node2, node3, newNode1, newNode2, newNode3) <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index
      addEdgeByIndex graph node1.index node3.index
      addEdgeByIndex graph node2.index node2.index
      newNode1 <- getNodeByIndex graph node1.index
      newNode2 <- getNodeByIndex graph node2.index
      newNode3 <- getNodeByIndex graph node3.index
      pure (node1, node2, node3, newNode1, newNode2, newNode3)
    fmap extractEdge newNode1.outEdges
      `shouldBe` V.fromList [(node1.index, node2.index, B.InterferenceGraphEdgeLabel), (node1.index, node3.index, B.InterferenceGraphEdgeLabel)]
    fmap extractEdge newNode2.outEdges
      `shouldBe` V.fromList [(node2.index, node1.index, B.InterferenceGraphEdgeLabel), (node2.index, node2.index, B.InterferenceGraphEdgeLabel)]
    fmap extractEdge newNode3.outEdges
      `shouldBe` V.fromList [(node3.index, node1.index, B.InterferenceGraphEdgeLabel)]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> getEdges" $ do
    (node1, node2, node3, edge) <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index
      addEdgeByIndex graph node1.index node2.index
      edge <- getEdgeByIndex graph node1.index node2.index
      pure (node1, node2, node3, edge)
    fmap extractEdge edge
      `shouldBe` Just (node1.index, node2.index, B.InterferenceGraphEdgeLabel)

  it "empty -> addNode*3 -> addEdgeByIndex*3 -> removeNode -> getNode" $ do
    (node1, node3) <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index
      addEdgeByIndex graph node1.index node3.index
      addEdgeByIndex graph node2.index node3.index
      removeNode graph node2
      node1 <- getNode graph 1
      node3 <- getNode graph 3
      pure (node1, node3)
    node1.val.vars `shouldBe` Set.singleton 1
    node3.val.vars `shouldBe` Set.singleton 3
    fmap extractEdge node1.outEdges `shouldBe` V.fromList [(NodeIndex 0, NodeIndex 1, B.InterferenceGraphEdgeLabel)]
    fmap extractEdge node3.outEdges `shouldBe` V.fromList [(NodeIndex 1, NodeIndex 0, B.InterferenceGraphEdgeLabel)]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> removeEdge -> getAllNodes" $ do
    (node1, node3, nodes) <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      edge <- addEdgeByIndex graph node1.index node2.index
      addEdgeByIndex graph node1.index node3.index
      addEdgeByIndex graph node2.index node3.index
      removeEdge graph edge
      nodes <- getAllNodes graph
      pure (node1, node3, nodes)
    fmap (.val.vars) nodes `shouldBe` V.fromList [Set.singleton 1, Set.singleton 2, Set.singleton 3]
    fmap (fmap extractEdge . getField @"outEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(NodeIndex 0, NodeIndex 2, B.InterferenceGraphEdgeLabel)],
          V.fromList [(NodeIndex 1, NodeIndex 2, B.InterferenceGraphEdgeLabel)],
          V.fromList [(NodeIndex 2, NodeIndex 0, B.InterferenceGraphEdgeLabel), (NodeIndex 2, NodeIndex 1, B.InterferenceGraphEdgeLabel)]
        ]

  it "empty -> addNode*2 -> updateNode(addMove) -> getAllNodes" $ do
    nodes <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      let move = B.newMove 1 2
      addMove graph move
      nodes <- getAllNodes graph
      pure nodes
    fmap (.val.vars) nodes `shouldBe` V.fromList [Set.singleton 1, Set.singleton 2]
    fmap (.val.isMoveRelated) nodes `shouldBe` V.fromList [True, True]

  it "empty -> addNode*2 -> updateNode(constrainMove) -> getAllNodes" $ do
    nodes <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      let move = B.newMove 1 2
      constrainMove graph move
      nodes <- getAllNodes graph
      pure nodes
    fmap (.val.vars) nodes `shouldBe` V.fromList [Set.singleton 1, Set.singleton 2]
    fmap (.val.isMoveRelated) nodes `shouldBe` V.fromList [False, False]

  it "empty -> addNode*3 -> addEdgeByIndex*3 -> updateNode(addMove) -> coalesceMove -> getAllNodes" $ do
    (node1, node2, nodes) <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node3.index
      addEdgeByIndex graph node2.index node3.index
      let move = B.newMove 1 2
          newNode1 = B.addMove move node1.val
          newNode2 = B.addMove move node2.val
      updateNode graph node1.index newNode1
      updateNode graph node2.index newNode2
      coalesceMove graph move
      nodes <- getAllNodes graph
      node1 <- getNode graph 1
      node2 <- getNode graph 2
      pure (node1, node2, nodes)
    node1 `shouldBe` node2
    fmap (.val.vars) nodes `shouldBe` V.fromList [Set.fromList [1, 2], Set.singleton 3]
    fmap (.val.isMoveRelated) nodes `shouldBe` V.fromList [False, False]
    fmap (fmap extractEdge . getField @"outEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(NodeIndex 0, NodeIndex 1, B.InterferenceGraphEdgeLabel)],
          V.fromList [(NodeIndex 1, NodeIndex 0, B.InterferenceGraphEdgeLabel)]
        ]

  it "empty -> addNode*2 -> updateNode(addMove) -> freezeMove -> getAllNodes" $ do
    nodes <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      let move = B.newMove 1 2
          newNode1 = B.addMove move node1.val
          newNode2 = B.addMove move node2.val
      updateNode graph node1.index newNode1
      updateNode graph node2.index newNode2
      freezeMove graph move
      nodes <- getAllNodes graph
      pure nodes
    fmap (.val.vars) nodes `shouldBe` V.fromList [Set.singleton 1, Set.singleton 2]
    fmap (.val.isMoveRelated) nodes `shouldBe` V.fromList [False, False]

freezeThawSpec :: Spec
freezeThawSpec = describe "freeze thaw spec" $ do
  it "thaw . freeze = id" $ do
    nodes <- do
      graph <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index
      addEdgeByIndex graph node1.index node3.index
      getAllNodes =<< thaw =<< freeze graph
    V.length nodes `shouldBe` 3
    fmap (.val.vars) nodes `shouldBe` V.fromList [Set.singleton 1, Set.singleton 2, Set.singleton 3]
    fmap (fmap extractEdge . getField @"outEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(NodeIndex 0, NodeIndex 1, B.InterferenceGraphEdgeLabel), (NodeIndex 0, NodeIndex 2, B.InterferenceGraphEdgeLabel)],
          V.fromList [(NodeIndex 1, NodeIndex 0, B.InterferenceGraphEdgeLabel)],
          V.fromList [(NodeIndex 2, NodeIndex 0, B.InterferenceGraphEdgeLabel)]
        ]

extractEdge :: Edge edge -> (NodeIndex, NodeIndex, edge)
extractEdge edge = (edge.source, edge.target, edge.val)
