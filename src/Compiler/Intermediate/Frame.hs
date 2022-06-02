module Compiler.Intermediate.Frame
  ( Frame (..),
    ProgramFragment (..),
    ProgramFragments (..),
    ProgramEff,
    runProgramEff,
    saveFragmentEff,
    saveMainFragmentEff,
    saveStringFragmentEff,
  )
where

import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible
import Data.Extensible.Effect
import RIO

class Frame f where
  type Access f = a | a -> f
  newFrame :: Lookup xs "temp" U.UniqueEff => U.Label -> [Bool] -> Eff xs f
  name :: f -> U.Label
  formals :: f -> [Access f]
  allocLocal :: (Lookup xs "temp" U.UniqueEff) => f -> Bool -> Eff xs (f, Access f)
  fp :: U.Temp
  rv :: U.Temp
  exp :: Access f -> IR.Exp -> IR.Exp
  wordSize :: Int
  externalCall :: Lookup xs "label" U.UniqueEff => String -> [IR.Exp] -> Eff xs IR.Exp
  procEntryExit1 :: f -> IR.Stm -> IR.Stm

data ProgramFragment f where
  Proc :: Frame f => {body :: IR.Stm, frame :: f} -> ProgramFragment f
  String :: {label :: U.Label, string :: String} -> ProgramFragment f

deriving instance Eq f => Eq (ProgramFragment f)

deriving instance Show f => Show (ProgramFragment f)

data ProgramFragments f = ProgramFragments {main :: ProgramFragment f, fragments :: [ProgramFragment f]}

deriving instance Eq f => Eq (ProgramFragments f)

deriving instance Show f => Show (ProgramFragments f)

emptyFragments :: ProgramFragments f
emptyFragments = ProgramFragments {main = undefined, fragments = []}

putMainFragment :: ProgramFragment f -> ProgramFragments f -> ProgramFragments f
putMainFragment fragment fragments = fragments {main = fragment}

addFragment :: ProgramFragment f -> ProgramFragments f -> ProgramFragments f
addFragment fragment fragments = fragments {fragments = fragments.fragments ++ [fragment]}

type ProgramEff f = State (ProgramFragments f)

runProgramEff :: Eff ((k >: (ProgramEff f)) ': xs) a -> Eff xs (a, ProgramFragments f)
runProgramEff = flip runStateEff emptyFragments

saveFragmentEff :: (Frame f, Lookup xs "fragment" (ProgramEff f)) => f -> IR.Stm -> Eff xs ()
saveFragmentEff frame stm = modifyEff #fragment . addFragment $ Proc {body = stm, frame = frame}

saveMainFragmentEff :: (Frame f, Lookup xs "fragment" (ProgramEff f)) => f -> IR.Stm -> Eff xs ()
saveMainFragmentEff frame stm = modifyEff #fragment . putMainFragment $ Proc {body = stm, frame = frame}

saveStringFragmentEff :: Lookup xs "fragment" (ProgramEff f) => U.Label -> String -> Eff xs ()
saveStringFragmentEff label string = modifyEff #fragment . addFragment $ String label string
