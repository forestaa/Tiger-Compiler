module Compiler.Intermediate.Unique.TestUtils
  ( newNthUnique,
    newNthTemp,
    newNthLabel,
    newNthNamedLabel,
  )
where

import Compiler.Intermediate.Unique qualified as U (Label, Temp, Unique, evalUniqueEff, getUniqueEff, namedLabel, newLabel, newTemp)
import Control.Monad (forM_)
import Data.Extensible.Effect (leaveEff)
import RIO

newNthUnique :: Int -> U.Unique
newNthUnique n = leaveEff . U.evalUniqueEff @"unique" $ do
  forM_ [1 .. n] . const $ U.getUniqueEff #unique
  U.getUniqueEff #unique

newNthTemp :: Int -> U.Temp
newNthTemp n = leaveEff . U.evalUniqueEff @"temp" $ do
  forM_ [1 .. n] $ const U.newTemp
  U.newTemp

newNthLabel :: Int -> U.Label
newNthLabel n = leaveEff . U.evalUniqueEff @"label" $ do
  forM_ [1 .. n] $ const U.newLabel
  U.newLabel

newNthNamedLabel :: Text -> Int -> U.Label
newNthNamedLabel body n = leaveEff . U.evalUniqueEff @"label" $ do
  forM_ [1 .. n] $ const U.newLabel
  U.namedLabel body
