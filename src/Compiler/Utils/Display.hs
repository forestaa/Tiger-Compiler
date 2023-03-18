module Compiler.Utils.Display where

import RIO (Display (..), Monoid (..), Semigroup (..), ($), (.), (<$>))
import RIO.List (intersperse)
import RIO.Set qualified as Set (Set, toList)

instance Display a => Display [a] where
  display list = "[" <> display_ list <> "]"
    where
      display_ list = mconcat . intersperse "," $ display <$> list

instance Display a => Display (Set.Set a) where
  display set = "fromList " <> display (Set.toList set)
