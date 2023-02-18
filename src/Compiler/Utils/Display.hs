module Compiler.Utils.Display where

import RIO (Display (..), Monoid (..), Semigroup (..), ($), (.), (<$>))
import RIO.List (intersperse)

instance Display a => Display [a] where
  display list = "[" <> display_ list <> "]"
    where
      display_ list = mconcat . intersperse "," $ display <$> list
