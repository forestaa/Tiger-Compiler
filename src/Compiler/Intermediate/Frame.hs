module Compiler.Intermediate.Frame
  ( Frame (..),
    Procedure (..),
    StringFragment (..),
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
import GHC.Records (HasField (..))
import RIO

class Frame f where
  type Access f = a | a -> f
  newFrame :: (Lookup xs "temp" U.UniqueEff) => U.Label -> [Bool] -> Eff xs f
  name :: f -> U.Label
  formals :: f -> [Access f]
  allocLocal :: (Lookup xs "temp" U.UniqueEff) => f -> Bool -> Eff xs (f, Access f)
  fp :: U.Temp
  rv :: U.Temp
  exp :: Access f -> IR.Exp -> IR.Exp
  wordSize :: Int
  externalCall :: (Lookup xs "label" U.UniqueEff) => Text -> [IR.Exp] -> Eff xs IR.Exp
  procEntryExit1 :: f -> IR.Stm -> IR.Stm

data Procedure f body = (Frame f) => Procedure {frame :: f, body :: body}

deriving instance (Eq f, Eq body) => Eq (Procedure f body)

deriving instance (Show f, Show body) => Show (Procedure f body)

data StringFragment = StringFragment {name :: U.Label, text :: Text} deriving (Show, Eq)

data ProgramFragment f where
  Proc :: (Frame f) => Procedure f IR.Stm -> ProgramFragment f
  String :: StringFragment -> ProgramFragment f

instance HasField "procedure" (ProgramFragment f) (Maybe (Procedure f IR.Stm)) where
  getField (Proc procedure) = Just procedure
  getField _ = Nothing

instance HasField "string" (ProgramFragment f) (Maybe StringFragment) where
  getField (String string) = Just string
  getField _ = Nothing

deriving instance (Eq f) => Eq (ProgramFragment f)

deriving instance (Show f) => Show (ProgramFragment f)

data ProgramFragments f = ProgramFragments {main :: ProgramFragment f, fragments :: [ProgramFragment f]}

deriving instance (Eq f) => Eq (ProgramFragments f)

deriving instance (Show f) => Show (ProgramFragments f)

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
saveFragmentEff frame stm = modifyEff #fragment . addFragment . Proc $ Procedure {body = stm, frame = frame}

saveMainFragmentEff :: (Frame f, Lookup xs "fragment" (ProgramEff f)) => f -> IR.Stm -> Eff xs ()
saveMainFragmentEff frame stm = modifyEff #fragment . putMainFragment . Proc $ Procedure {body = stm, frame = frame}

saveStringFragmentEff :: (Lookup xs "fragment" (ProgramEff f)) => U.Label -> Text -> Eff xs ()
saveStringFragmentEff label string = modifyEff #fragment . addFragment . String $ StringFragment label string
