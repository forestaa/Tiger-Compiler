module Compiler.Utils.String (unlines) where

import RIO hiding (unlines)

unlines :: (Foldable t, Monoid s, IsString s) => t s -> s
unlines = foldr ($$) mempty

($$) :: (Monoid s, IsString s) => s -> s -> s
($$) = mid newline

char :: IsString s => Char -> s
char c = fromString [c]

newline :: IsString s => s
newline = char '\n'

mid :: Monoid s => s -> (s -> s -> s)
mid m x y = between x y m

between :: (Monoid s) => s -> s -> (s -> s)
between open close x = open <> x <> close
