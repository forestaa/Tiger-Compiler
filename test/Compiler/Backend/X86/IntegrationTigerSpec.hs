module Compiler.Backend.X86.IntegrationTigerSpec (spec) where

import Compiler.Backend.X86.Arch
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.Frame (Frame, ProcedureX86 (..), ProgramFragmentX86 (..), StringFragmentX86 (..), procEntryExit3, r8, r9, rax, rbp, rcx, rdi, rdx, rip, rsi, rsp)
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (val))
import Compiler.Backend.X86.RegisterAllocation (allocateRegisters)
import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Language.Tiger (Tiger (Tiger))
import Compiler.Frontend.Language.Tiger.Samples (tigerTest)
import Compiler.Intermediate.Canonical (Canonical (Canonical))
import Compiler.Intermediate.Frame qualified as F (ProgramFragment (..))
import Compiler.Intermediate.Unique qualified as U
import Compiler.Utils.Maybe ()
import Control.Exception.Safe (throwM)
import Data.ByteString.Lazy qualified as B
import Data.Either (either)
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff, liftEff, mapLeftEff, runEitherEff, throwEff)
import Data.Extensible.Effect.Default (runIODef)
import GHC.Records (HasField (getField))
import RIO hiding (throwM)
import RIO.List.Partial ((!!))
import Test.Hspec

spec :: Spec
spec = integrationSpec

integrationSpec :: Spec
integrationSpec = describe "integration spec for x86 backend of tiger" $ do
  it "test01.tig" $ do
    let testcase = tigerTest "test01.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.Label "tigerMain" (U.Unique 12)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        label13 = fromUniqueLabel $ U.Label "L" (U.Unique 13)
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Label mainLabel,
          MovRegister RSP RBP,
          MovImmediate 10 RAX,
          MovImmediate 0 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call (fromUniqueLabel (U.Label "initArray" (U.Unique 11))),
          MovRegister RAX RAX,
          MovRegister RAX RAX,
          Jump label13,
          Label label13,
          Leave,
          Ret
        ]

  it "test02.tig" $ do
    let testcase = tigerTest "test02.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.Label "tigerMain" (U.Unique 12)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        label13 = fromUniqueLabel $ U.Label "L" (U.Unique 13)
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Label mainLabel,
          MovRegister RSP RBP,
          MovImmediate 10 RAX,
          MovImmediate 0 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call (fromUniqueLabel (U.Label "initArray" (U.Unique 11))),
          MovRegister RAX RAX,
          MovRegister RAX RAX,
          Jump label13,
          Label label13,
          Leave,
          Ret
        ]

  it "test03.tig" $ do
    let testcase = tigerTest "test03.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.Label "tigerMain" (U.Unique 14)
        label11 = fromUniqueLabel $ U.Label "L" (U.Unique 11)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        label13 = fromUniqueLabel $ U.Label "L" (U.Unique 13)
        label14 = fromUniqueLabel $ U.Label "L" (U.Unique 14)
        label15 = fromUniqueLabel $ U.Label "L" (U.Unique 15)
    length result `shouldBe` 3
    (result !! 0).string.body
      `shouldBe` Just
        [ Text,
          Global label11,
          Data,
          Align 16,
          Type label11,
          Size label11 24,
          Label label11,
          Long 8,
          Compiler.Backend.X86.Arch.String "\"Nobody\"",
          Zero 7
        ]
    (result !! 1).string.body
      `shouldBe` Just
        [ Text,
          Global label13,
          Data,
          Align 16,
          Type label13,
          Size label13 24,
          Label label13,
          Long 10,
          Compiler.Backend.X86.Arch.String "\"Somebody\"",
          Zero 5
        ]
    (result !! 2).procedure.body
      `shouldBe` Just
        [ Label mainLabel,
          MovRegister RSP RBP,
          MovImmediate 16 RAX,
          MovRegister RAX RDI,
          Call (fromUniqueLabel (U.Label "malloc" (U.Unique 12))),
          MovRegister RAX RCX,
          Lea label11 RIP RAX,
          MovStoreIndirect RAX 0 RCX,
          MovImmediate 1000 RAX,
          MovStoreIndirect RAX 8 RCX,
          MovRegister RCX RCX,
          Lea label13 RIP RAX,
          MovStoreIndirect RAX 0 RCX,
          Jump label15,
          Label label15,
          Leave,
          Ret
        ]

  it "test04.tig" $ do
    let testcase = tigerTest "test04.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.Label "tigerMain" (U.Unique 17)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        label13 = fromUniqueLabel $ U.Label "L" (U.Unique 13)
        label14 = fromUniqueLabel $ U.Label "L" (U.Unique 14)
        label15 = fromUniqueLabel $ U.Label "L" (U.Unique 15)
        label16 = fromUniqueLabel $ U.Label "L" (U.Unique 16)
        label17 = fromUniqueLabel $ U.Label "L" (U.Unique 17)
        label18 = fromUniqueLabel $ U.Label "L" (U.Unique 18)
        nfactor = fromUniqueLabel $ U.Label "nfactor" (U.Unique 11)
    length result `shouldBe` 2
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Label label12,
          MovImmediate 1 RAX,
          Label label14,
          MovRegister RAX RAX,
          Jump label15,
          Label label13,
          MovRegister RCX RDX,
          MovLoadIndirect 0 RBP RAX,
          MovRegister RAX RAX,
          MovRegister RCX RCX,
          SubImmediate 1 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call nfactor,
          MovRegister RAX RCX,
          MovRegister RDX RAX,
          MulRegister RCX RAX,
          MovRegister RAX RAX,
          Jump label14,
          Label nfactor,
          MovRegister RSP RBP,
          MovStoreIndirect RDI 0 RBP,
          MovRegister RSI RCX,
          CmpImmediate RCX 0,
          JumpIfEqual label12,
          Label label16,
          Jump label13,
          Label label15,
          Leave,
          Ret
        ]
    (result !! 1).procedure.body
      `shouldBe` Just
        [ Label mainLabel,
          MovRegister RSP RBP,
          MovRegister RBP RAX,
          MovImmediate 10 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call nfactor,
          Jump label18,
          Label label18,
          Leave,
          Ret
        ]

compileTest :: FilePath -> IO [ProgramFragmentX86 [Assembly Register]]
compileTest file = (=<<) (either throwM pure) . runIODef . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" . runEitherEff @"exception" $ do
  bs <- liftEff (Proxy @"IO") $ B.readFile file
  fragments <- (=<<) (either (throwEff #exception . toException) pure) . runEitherEff @"frontendException" $ processFrontend @Tiger @Frame file bs
  fragments <- codegen @Canonical fragments
  mapM allocateRegisterOverFragments fragments
  where
    allocateRegisterOverFragments :: Lookup xs "temp" U.UniqueEff => ProgramFragmentX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProgramFragmentX86 [Assembly Register])
    allocateRegisterOverFragments (Proc procedure) = Proc . procEntryExit3 <$> allocateRegisters procedure
    allocateRegisterOverFragments (Compiler.Backend.X86.Frame.String (StringFragment strings)) = pure . Compiler.Backend.X86.Frame.String . StringFragment $ fmap (replaceRegister undefined . getField @"val") strings
