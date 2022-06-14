module Compiler.Frontend.Language.Tiger.Samples where

import RIO
import Text.Printf (printf)

tigerTest :: String -> FilePath
tigerTest file = "test/Compiler/Frontend/Language/Tiger/samples/" ++ file

validTigerTests :: [FilePath]
validTigerTests =
  let validTestCases = printf "test%02d.tig" <$> (concat [[1 .. 8], [12], [27], [30], [37], [41 .. 42], [44], [46 .. 48]] :: [Int])
   in map tigerTest validTestCases
