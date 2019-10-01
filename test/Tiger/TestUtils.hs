module Tiger.TestUtils where

import Unique

import Data.Extensible
import RIO

fetchTwoLabel :: (Label, Label)
fetchTwoLabel = leaveEff . runUniqueEff @"label" $ (,) <$> newLabel <*> newLabel
