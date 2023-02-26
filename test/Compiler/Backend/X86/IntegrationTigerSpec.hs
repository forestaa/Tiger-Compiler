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
import Compiler.Intermediate.Unique.TestUtils (newNthLabel, newNthNamedLabel)
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
    let mainLabel = fromUniqueLabel $ U.mainLabel "tigermain"
        label11 = fromUniqueLabel $ newNthNamedLabel "L" 11
        initArrayLabel = newNthNamedLabel "initArray" 10
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Global mainLabel,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          MovImmediate 10 RAX,
          MovImmediate 0 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call (fromUniqueLabel initArrayLabel),
          MovRegister RAX RAX,
          MovRegister RAX RAX,
          Jump label11,
          Label label11,
          Leave,
          Ret
        ]

  it "test02.tig" $ do
    let testcase = tigerTest "test02.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.mainLabel "tigermain"
        label11 = fromUniqueLabel $ newNthLabel 11
        initArrayLabel = newNthNamedLabel "initArray" 10
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Global mainLabel,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          MovImmediate 10 RAX,
          MovImmediate 0 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call (fromUniqueLabel initArrayLabel),
          MovRegister RAX RAX,
          MovRegister RAX RAX,
          Jump label11,
          Label label11,
          Leave,
          Ret
        ]

  it "test03.tig" $ do
    let testcase = tigerTest "test03.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.mainLabel "tigermain"
        label10 = fromUniqueLabel $ newNthLabel 10
        label12 = fromUniqueLabel $ newNthLabel 12
        label13 = fromUniqueLabel $ newNthLabel 13
        mallocLabel = newNthNamedLabel "malloc" 11
    length result `shouldBe` 3
    (result !! 0).string.body
      `shouldBe` Just
        [ Text,
          Global label10,
          Data,
          Align 16,
          Type label10,
          Size label10 24,
          Label label10,
          Long 8,
          Compiler.Backend.X86.Arch.String "\"Nobody\"",
          Zero 7
        ]
    (result !! 1).string.body
      `shouldBe` Just
        [ Text,
          Global label12,
          Data,
          Align 16,
          Type label12,
          Size label12 24,
          Label label12,
          Long 10,
          Compiler.Backend.X86.Arch.String "\"Somebody\"",
          Zero 5
        ]
    (result !! 2).procedure.body
      `shouldBe` Just
        [ Global mainLabel,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          MovImmediate 16 RAX,
          MovRegister RAX RDI,
          Call (fromUniqueLabel mallocLabel),
          MovRegister RAX RCX,
          Lea label10 RIP RAX,
          MovStoreIndirect RAX 0 RCX,
          MovImmediate 1000 RAX,
          MovStoreIndirect RAX 8 RCX,
          MovRegister RCX RCX,
          Lea label12 RIP RAX,
          MovStoreIndirect RAX 0 RCX,
          Jump label13,
          Label label13,
          Leave,
          Ret
        ]

  it "test04.tig" $ do
    let testcase = tigerTest "test04.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.mainLabel "tigermain"
        label11 = fromUniqueLabel $ newNthLabel 11
        label12 = fromUniqueLabel $ newNthLabel 12
        label13 = fromUniqueLabel $ newNthLabel 13
        label14 = fromUniqueLabel $ newNthLabel 14
        label15 = fromUniqueLabel $ newNthLabel 15
        label16 = fromUniqueLabel $ newNthLabel 16
        nfactor = fromUniqueLabel $ newNthNamedLabel "nfactor" 10
    length result `shouldBe` 2
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Global nfactor,
          Label label11,
          MovImmediate 1 RAX,
          Label label13,
          MovRegister RAX RAX,
          Jump label14,
          Label label12,
          MovRegister RCX RAX,
          MovStoreIndirect RAX (-8) RBP,
          MovLoadIndirect 0 RBP RAX,
          MovRegister RAX RAX,
          MovRegister RCX RCX,
          SubImmediate 1 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call nfactor,
          MovRegister RAX RCX,
          MovLoadIndirect (-8) RBP RAX,
          MovRegister RAX RAX,
          MulRegister RCX RAX,
          MovRegister RAX RAX,
          Jump label13,
          Label nfactor,
          PushRegister RBP,
          MovRegister RSP RBP,
          SubImmediate 8 RSP,
          MovStoreIndirect RDI 0 RBP,
          MovRegister RSI RCX,
          CmpImmediate RCX 0,
          JumpIfEqual label11,
          Label label15,
          Jump label12,
          Label label14,
          Leave,
          Ret
        ]
    (result !! 1).procedure.body
      `shouldBe` Just
        [ Global mainLabel,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          MovRegister RBP RAX,
          MovImmediate 10 RCX,
          MovRegister RAX RDI,
          MovRegister RCX RSI,
          Call nfactor,
          Jump label16,
          Label label16,
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
