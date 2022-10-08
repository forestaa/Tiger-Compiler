module Compiler.Utils.Graph.ImmutableSpec (spec) where

import Compiler.Utils.Graph.Base
import Compiler.Utils.Graph.Immutable (ImmutableGraph (getNode), bfs)
import Compiler.Utils.Graph.Mutable qualified as Mutable (MGraph, MutableGraph (..), freeze)
import GHC.Records (HasField (getField))
import RIO
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  bfsSpec

bfsSpec :: Spec
bfsSpec = describe "bfs spec" $ do
  it "" $ do
    graph <- do
      -- 1 -> 2 -> 3
      --        -> 4 -> 5
      --   -> 3 -> 6
      graph :: Mutable.MGraph 'Directional _ _ _ <- Mutable.empty
      node1 <- Mutable.addNode graph 1
      node2 <- Mutable.addNode graph 2
      node3 <- Mutable.addNode graph 3
      node4 <- Mutable.addNode graph 4
      node5 <- Mutable.addNode graph 5
      node6 <- Mutable.addNode graph 6
      Mutable.addEdgeByIndex graph node1.index node2.index ()
      Mutable.addEdgeByIndex graph node1.index node3.index ()
      Mutable.addEdgeByIndex graph node2.index node3.index ()
      Mutable.addEdgeByIndex graph node2.index node4.index ()
      Mutable.addEdgeByIndex graph node4.index node5.index ()
      Mutable.addEdgeByIndex graph node3.index node6.index ()
      Mutable.freeze graph
    let node = getNode graph 1
    fmap (getField @"val") (bfs graph [node]) `shouldBe` [1, 2, 3, 4, 6, 5]
