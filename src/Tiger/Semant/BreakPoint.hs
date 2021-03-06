module Tiger.Semant.BreakPoint where

import           Data.Extensible
import           RIO
import qualified RIO.List.Partial as Partial

import           Unique

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
      modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (done:breakpoints)
    popBreakPoint :: Eff xs ()
    popBreakPoint = modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (Partial.tail breakpoints)
fetchCurrentBreakPoint :: Lookup xs "breakpoint" BreakPointEff => Eff xs (Maybe Label)
fetchCurrentBreakPoint = getEff #breakpoint >>= \case
  BreakPointStack [] -> pure Nothing
  BreakPointStack (breakpoint:_) -> pure $ Just breakpoint
