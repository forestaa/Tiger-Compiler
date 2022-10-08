module Compiler.Utils.Graph.MutableSpec (spec) where

import Compiler.Utils.Graph.Base
import Compiler.Utils.Graph.Mutable
import Compiler.Utils.Maybe
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Vector qualified as V
import GHC.Records (HasField (getField))
import RIO hiding (reverse)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  mgraphDirectionalSpec
  mgraphUnDirectionalSpec
  freezeThawSpec

mgraphDirectionalSpec :: Spec
mgraphDirectionalSpec = describe "MGraph 'Directional spec" $ do
  it "empty -> getNode -> KeyNotFound Exception" $ do
    shouldThrow
      ( do
          graph :: MGraph 'Directional _ _ _ <- empty
          getNode graph 1
      )
      (KeyNotFound ==)

  it "empty -> addNode -> getNode" $ do
    node <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      addNode graph 1
      getNode graph 1
    node.val `shouldBe` 1

  it "empty -> addNode*2(same) -> KeyAlreadyExist Exception" $ do
    shouldThrow
      ( do
          graph :: MGraph 'Directional _ _ _ <- empty
          addNode graph 1
          addNode graph 1
          pure ()
      )
      (KeyAlradyExist ==)

  it "empty -> addNode -> getNodeByIndex" $ do
    node <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      node <- addNode graph 1
      getNodeByIndex graph node.index
    node.val `shouldBe` 1

  it "empty -> addNode*3 -> getAllNodes" $ do
    nodes <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      addNode graph 1
      addNode graph 2
      addNode graph 3
      getAllNodes graph
    V.length nodes `shouldBe` 3
    (nodes V.!? 0).val `shouldBe` Just 1
    (nodes V.!? 1).val `shouldBe` Just 2
    (nodes V.!? 2).val `shouldBe` Just 3

  it "empty -> addEdge -> KeyNotFound Exception" $ do
    shouldThrow
      ( do
          graph :: MGraph 'Directional _ _ _ <- empty
          addEdge graph 1 2 ()
      )
      (KeyNotFound ==)

  it "empty -> addNode*3 -> addEdge*2 -> getEdges" $ do
    (node1, node2, node3, edges) <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdge graph 1 2 ()
      addEdge graph 1 2 ()
      edges <- getEdgesByIndex graph node1.index node2.index
      pure (node1, node2, node3, edges)
    fmap extractEdge edges
      `shouldBe` V.fromList [(node1.index, node2.index, ()), (node1.index, node2.index, ())]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> node.outEdges, node.inEdges" $ do
    (node1, node2, node3, newNode1, newNode2, newNode3) <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node3.index ()
      newNode1 <- getNodeByIndex graph node1.index
      newNode2 <- getNodeByIndex graph node2.index
      newNode3 <- getNodeByIndex graph node3.index
      pure (node1, node2, node3, newNode1, newNode2, newNode3)
    fmap extractEdge newNode1.outEdges
      `shouldBe` V.fromList [(node1.index, node2.index, ()), (node1.index, node3.index, ())]
    fmap extractEdge newNode1.inEdges
      `shouldBe` V.fromList []
    fmap extractEdge newNode2.outEdges
      `shouldBe` V.fromList []
    fmap extractEdge newNode2.inEdges
      `shouldBe` V.fromList [(node1.index, node2.index, ())]
    fmap extractEdge newNode3.outEdges
      `shouldBe` V.fromList []
    fmap extractEdge newNode3.inEdges
      `shouldBe` V.fromList [(node1.index, node3.index, ())]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> getEdges" $ do
    (node1, node2, node3, edges) <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node2.index ()
      edges <- getEdgesByIndex graph node1.index node2.index
      pure (node1, node2, node3, edges)
    fmap extractEdge edges
      `shouldBe` V.fromList [(node1.index, node2.index, ()), (node1.index, node2.index, ())]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> removeNode -> getAllNodes" $ do
    (node1, node3, nodes) <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node3.index ()
      removeNode graph node2
      nodes <- getAllNodes graph
      pure (node1, node3, nodes)
    fmap (getField @"val") nodes `shouldBe` V.fromList [1, 3]
    fmap (fmap extractEdge . getField @"outEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(NodeIndex 0, NodeIndex 1, ())],
          V.fromList []
        ]
    fmap (fmap extractEdge . getField @"inEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [],
          V.fromList [(NodeIndex 0, NodeIndex 1, ())]
        ]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> reverse -> getAllNodes" $ do
    (node1, node2, node3, nodes) <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node3.index ()
      reversed <- reverse graph
      nodes <- getAllNodes reversed
      pure (node1, node2, node3, nodes)
    fmap (getField @"val") nodes `shouldBe` V.fromList [1, 2, 3]
    fmap (fmap extractEdge . getField @"outEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [],
          V.fromList [(node2.index, node1.index, ())],
          V.fromList [(node3.index, node1.index, ())]
        ]
    fmap (fmap extractEdge . getField @"inEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(node2.index, node1.index, ()), (node3.index, node1.index, ())],
          V.fromList [],
          V.fromList []
        ]

mgraphUnDirectionalSpec :: Spec
mgraphUnDirectionalSpec = describe "MGraph 'UnDirectional spec" $ do
  it "empty -> getNode -> KeyNotFound Exception" $ do
    shouldThrow
      ( do
          graph :: MGraph 'UnDirectional _ _ _ <- empty
          getNode graph 1
      )
      (KeyNotFound ==)

  it "empty -> addNode -> getNode" $ do
    node <- do
      graph :: MGraph 'UnDirectional _ _ _ <- empty
      addNode graph 1
      getNode graph 1
    node.val `shouldBe` 1

  it "empty -> addNode*2(same) -> KeyAlreadyExist Exception" $ do
    shouldThrow
      ( do
          graph :: MGraph 'UnDirectional _ _ _ <- empty
          addNode graph 1
          addNode graph 1
          pure ()
      )
      (KeyAlradyExist ==)

  it "empty -> addNode -> getNodeByIndex" $ do
    node <- do
      graph :: MGraph 'UnDirectional _ _ _ <- empty
      node <- addNode graph 1
      getNodeByIndex graph node.index
    node.val `shouldBe` 1

  it "empty -> addNode*3 -> getAllNodes" $ do
    nodes <- do
      graph :: MGraph 'UnDirectional _ _ _ <- empty
      addNode graph 1
      addNode graph 2
      addNode graph 3
      getAllNodes graph
    V.length nodes `shouldBe` 3
    (nodes V.!? 0).val `shouldBe` Just 1
    (nodes V.!? 1).val `shouldBe` Just 2
    (nodes V.!? 2).val `shouldBe` Just 3

  it "empty -> addEdge -> KeyNotFound Exception" $ do
    shouldThrow
      ( do
          graph :: MGraph 'UnDirectional _ _ _ <- empty
          addEdge graph 1 2 ()
      )
      (KeyNotFound ==)

  it "empty -> addNode*3 -> addEdge*2 -> getEdges" $ do
    (node1, node2, node3, edges, edges') <- do
      graph :: MGraph 'UnDirectional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdge graph 1 2 ()
      addEdge graph 1 2 ()
      edges <- getEdgesByIndex graph node1.index node2.index
      edges' <- getEdgesByIndex graph node2.index node1.index
      pure (node1, node2, node3, edges, edges')
    fmap extractEdge edges
      `shouldBe` V.fromList [(node1.index, node2.index, ()), (node1.index, node2.index, ())]
    fmap extractEdge edges'
      `shouldBe` V.fromList [(node2.index, node1.index, ()), (node2.index, node1.index, ())]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> node.outEdges, node.inEdges" $ do
    (node1, node2, node3, newNode1, newNode2, newNode3) <- do
      graph :: MGraph 'UnDirectional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node3.index ()
      addEdgeByIndex graph node2.index node2.index ()
      newNode1 <- getNodeByIndex graph node1.index
      newNode2 <- getNodeByIndex graph node2.index
      newNode3 <- getNodeByIndex graph node3.index
      pure (node1, node2, node3, newNode1, newNode2, newNode3)
    fmap extractEdge newNode1.outEdges
      `shouldBe` V.fromList [(node1.index, node2.index, ()), (node1.index, node3.index, ())]
    fmap extractEdge newNode1.inEdges
      `shouldBe` V.fromList [(node2.index, node1.index, ()), (node3.index, node1.index, ())]
    fmap extractEdge newNode2.outEdges
      `shouldBe` V.fromList [(node2.index, node1.index, ()), (node2.index, node2.index, ())]
    fmap extractEdge newNode2.inEdges
      `shouldBe` V.fromList [(node1.index, node2.index, ()), (node2.index, node2.index, ())]
    fmap extractEdge newNode3.outEdges
      `shouldBe` V.fromList [(node3.index, node1.index, ())]
    fmap extractEdge newNode3.inEdges
      `shouldBe` V.fromList [(node1.index, node3.index, ())]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> getEdges" $ do
    (node1, node2, node3, edges) <- do
      graph :: MGraph 'UnDirectional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node2.index ()
      edges <- getEdgesByIndex graph node1.index node2.index
      pure (node1, node2, node3, edges)
    fmap extractEdge edges
      `shouldBe` V.fromList [(node1.index, node2.index, ()), (node1.index, node2.index, ())]

  it "empty -> addNode*3 -> addEdgeByIndex*2 -> removeNode -> getAllNodes" $ do
    (node1, node3, nodes) <- do
      graph :: MGraph 'UnDirectional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node3.index ()
      addEdgeByIndex graph node3.index node3.index ()
      removeNode graph node2
      nodes <- getAllNodes graph
      pure (node1, node3, nodes)
    fmap (getField @"val") nodes `shouldBe` V.fromList [1, 3]
    fmap (fmap extractEdge . getField @"outEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(NodeIndex 0, NodeIndex 1, ())],
          V.fromList [(NodeIndex 1, NodeIndex 0, ()), (NodeIndex 1, NodeIndex 1, ())]
        ]
    fmap (fmap extractEdge . getField @"inEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(NodeIndex 1, NodeIndex 0, ())],
          V.fromList [(NodeIndex 0, NodeIndex 1, ()), (NodeIndex 1, NodeIndex 1, ())]
        ]

freezeThawSpec :: Spec
freezeThawSpec = describe "freeze thaw spec" $ do
  it "thaw . freeze = id" $ do
    nodes <- do
      graph :: MGraph 'Directional _ _ _ <- empty
      node1 <- addNode graph 1
      node2 <- addNode graph 2
      node3 <- addNode graph 3
      addEdgeByIndex graph node1.index node2.index ()
      addEdgeByIndex graph node1.index node3.index ()
      getAllNodes =<< thaw =<< freeze graph
    V.length nodes `shouldBe` 3
    fmap (getField @"val") nodes `shouldBe` V.fromList [1, 2, 3]
    fmap (fmap extractEdge . getField @"outEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [(NodeIndex 0, NodeIndex 1, ()), (NodeIndex 0, NodeIndex 2, ())],
          V.fromList [],
          V.fromList []
        ]
    fmap (fmap extractEdge . getField @"inEdges") nodes
      `shouldBe` V.fromList
        [ V.fromList [],
          V.fromList [(NodeIndex 0, NodeIndex 1, ())],
          V.fromList [(NodeIndex 0, NodeIndex 2, ())]
        ]

extractEdge :: Edge edge -> (NodeIndex, NodeIndex, edge)
extractEdge edge = (edge.source, edge.target, edge.val)
