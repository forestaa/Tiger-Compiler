module Compiler.Backend.X86 (processBackend) where

import Compiler.Backend.X86.Arch (Assembly, Register, replaceRegister)
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.File (writeAssemblyFile)
import Compiler.Backend.X86.Frame (Frame, ProgramFragmentX86 (..), StringFragmentX86 (..), procEntryExit3)
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (val))
import Compiler.Backend.X86.RegisterAllocation (allocateRegisters)
import Compiler.Intermediate.Canonical (Canonical)
import Compiler.Intermediate.Frame qualified as F (ProgramFragments (..))
import Compiler.Intermediate.Unique qualified as U (Temp, UniqueEff)
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)
import GHC.Records (HasField (getField))
import RIO
import Compiler.Backend (Backend(..))


instance Backend Frame where
  processBackend :: (Lookup xs "temp" U.UniqueEff, Lookup xs "label" U.UniqueEff) => F.ProgramFragments Frame -> Eff xs Utf8Builder
  processBackend fragments = do
    fragments <- codegen @Canonical fragments
    fragments <- mapM allocateRegisterOverFragments fragments
    pure $ writeAssemblyFile fragments
    where
      allocateRegisterOverFragments :: Lookup xs "temp" U.UniqueEff => ProgramFragmentX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProgramFragmentX86 [Assembly Register])
      allocateRegisterOverFragments (Proc procedure) = Proc . procEntryExit3 <$> allocateRegisters procedure
      allocateRegisterOverFragments (Compiler.Backend.X86.Frame.String (StringFragment strings)) = pure . Compiler.Backend.X86.Frame.String . StringFragment $ fmap (replaceRegister undefined . getField @"val") strings
