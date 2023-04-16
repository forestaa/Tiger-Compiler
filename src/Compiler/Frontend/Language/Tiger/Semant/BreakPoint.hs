module Compiler.Frontend.Language.Tiger.Semant.BreakPoint where

import Compiler.Intermediate.Unique
import Data.Extensible
import Data.Extensible.Effect
import RIO
import RIO.List (headMaybe)
import RIO.List.Partial qualified as Partial

newtype BreakPointStack = BreakPointStack [Label]

type BreakPointEff = State BreakPointStack

runBreakPointEff :: Eff (("breakpoint" >: BreakPointEff) ': xs) a -> Eff xs a
runBreakPointEff = flip (evalStateEff @"breakpoint") (BreakPointStack [])

withBreakPoint :: forall xs a. (Lookup xs "label" UniqueEff, Lookup xs "breakpoint" BreakPointEff) => Eff xs a -> Eff xs a
withBreakPoint body = do
  pushNewBreakPoint
  ret <- body
  popBreakPoint
  pure ret
  where
    pushNewBreakPoint :: Eff xs ()
    pushNewBreakPoint = do
      done <- newLabel
      modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (done : breakpoints)
    popBreakPoint :: Eff xs ()
    popBreakPoint = modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (Partial.tail breakpoints)

fetchCurrentBreakPoint :: (Lookup xs "breakpoint" BreakPointEff) => Eff xs (Maybe Label)
fetchCurrentBreakPoint = getsEff #breakpoint $ \(BreakPointStack breakpoints) -> headMaybe breakpoints
