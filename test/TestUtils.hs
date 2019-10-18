module TestUtils where

import Data.Extensible
import RIO

import Unique


fetchTwoLabel :: (Label, Label)
fetchTwoLabel = leaveEff . runUniqueEff @"label" $ (,) <$> newLabel <*> newLabel
