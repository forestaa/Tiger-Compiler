module Compiler.Backend.X86.IntegrationTigerSpec (spec) where

import Compiler.Backend.X86.Arch
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.Frame (Frame, ProcedureX86 (..), ProgramFragmentX86 (..), StringFragmentX86 (..), procEntryExit3, r8, r9, rax, rbp, rcx, rdi, rdx, rip, rsi, rsp)
import Compiler.Backend.X86.Liveness qualified as L (ControlFlow (val))
import Compiler.Backend.X86.RegisterAllocation (RegisterAllocation (..))
import Compiler.Backend.X86.RegisterAllocation.CoalesceAllocation (CoalesceAllocation)
import Compiler.Backend.X86.RegisterAllocation.SimpleAllocation (SimpleAllocation)
import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Language.Tiger (Tiger)
import Compiler.Frontend.Language.Tiger.Samples (tigerTest)
import Compiler.Intermediate.Canonical (Canonical)
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
    let mainLabel = fromUniqueLabel $ U.externalLabel "tigermain"
        label0 = fromUniqueLabel $ newNthNamedLabel "L" 0
        initArrayLabel = fromUniqueLabel $ U.externalLabel "initArray"
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Text,
          Global mainLabel,
          Type mainLabel Function,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          SubImmediate 8 RSP,
          MovImmediate 10 RDI,
          MovImmediate 0 RSI,
          Call initArrayLabel,
          Jump label0,
          Label label0,
          Leave,
          Ret
        ]

  it "test02.tig" $ do
    let testcase = tigerTest "test02.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.externalLabel "tigermain"
        label0 = fromUniqueLabel $ newNthLabel 0
        initArrayLabel = fromUniqueLabel $ U.externalLabel "initArray"
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Text,
          Global mainLabel,
          Type mainLabel Function,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          SubImmediate 8 RSP,
          MovImmediate 10 RDI,
          MovImmediate 0 RSI,
          Call initArrayLabel,
          Jump label0,
          Label label0,
          Leave,
          Ret
        ]

  it "test03.tig" $ do
    let testcase = tigerTest "test03.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.externalLabel "tigermain"
        label0 = fromUniqueLabel $ newNthLabel 0
        label1 = fromUniqueLabel $ newNthLabel 1
        label2 = fromUniqueLabel $ newNthLabel 2
        mallocLabel = fromUniqueLabel $ U.externalLabel "malloc"
    length result `shouldBe` 3
    (result !! 0).string.body
      `shouldBe` Just
        [ Text,
          Global label0,
          Data,
          Align 16,
          Type label0 Object,
          Size label0 16,
          Label label0,
          Quad 6,
          Compiler.Backend.X86.Arch.String "Nobody",
          Zero 1
        ]
    (result !! 1).string.body
      `shouldBe` Just
        [ Text,
          Global label1,
          Data,
          Align 16,
          Type label1 Object,
          Size label1 16,
          Label label1,
          Quad 8,
          Compiler.Backend.X86.Arch.String "Somebody"
        ]
    (result !! 2).procedure.body
      `shouldBe` Just
        [ Text,
          Global mainLabel,
          Type mainLabel Function,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          SubImmediate 8 RSP,
          MovImmediate 16 RDI,
          Call mallocLabel,
          Lea label0 RIP RCX,
          MovStoreIndirect RCX 0 RAX,
          MovImmediate 1000 RCX,
          MovStoreIndirect RCX 8 RAX,
          Lea label1 RIP RCX,
          MovStoreIndirect RCX 0 RAX,
          Jump label2,
          Label label2,
          Leave,
          Ret
        ]

  it "test04.tig" $ do
    let testcase = tigerTest "test04.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.externalLabel "tigermain"
        label1 = fromUniqueLabel $ newNthLabel 1
        label2 = fromUniqueLabel $ newNthLabel 2
        label3 = fromUniqueLabel $ newNthLabel 3
        label4 = fromUniqueLabel $ newNthLabel 4
        label5 = fromUniqueLabel $ newNthLabel 5
        label6 = fromUniqueLabel $ newNthLabel 6
        nfactor = fromUniqueLabel $ newNthNamedLabel "nfactor" 0
    length result `shouldBe` 2
    (result !! 0).procedure.body
      `shouldBe` Just
        [ Text,
          Global nfactor,
          Type nfactor Function,
          Label label1,
          MovImmediate 1 RAX,
          Label label3,
          Jump label4,
          Label label2,
          MovRegister RSI RAX,
          MovStoreIndirect RAX (-16) RBP,
          MovLoadIndirect (-8) RBP RDI,
          SubImmediate 1 RSI,
          Call nfactor,
          MovLoadIndirect (-16) RBP RCX,
          MulRegister RAX RCX,
          MovRegister RCX RAX,
          Jump label3,
          Label nfactor,
          PushRegister RBP,
          MovRegister RSP RBP,
          SubImmediate 16 RSP,
          MovStoreIndirect RDI (-8) RBP,
          CmpImmediate RSI 0,
          JumpIfEqual label1,
          Label label5,
          Jump label2,
          Label label4,
          Leave,
          Ret
        ]
    (result !! 1).procedure.body
      `shouldBe` Just
        [ Text,
          Global mainLabel,
          Type mainLabel Function,
          Label mainLabel,
          PushRegister RBP,
          MovRegister RSP RBP,
          SubImmediate 8 RSP,
          MovRegister RBP RDI,
          MovImmediate 10 RSI,
          Call nfactor,
          Jump label6,
          Label label6,
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
    allocateRegisterOverFragments :: (Lookup xs "temp" U.UniqueEff) => ProgramFragmentX86 [L.ControlFlow U.Temp (Assembly U.Temp)] -> Eff xs (ProgramFragmentX86 [Assembly Register])
    allocateRegisterOverFragments (Proc procedure) = Proc . procEntryExit3 <$> allocateRegisters @CoalesceAllocation procedure
    allocateRegisterOverFragments (Compiler.Backend.X86.Frame.String (StringFragment strings)) = pure . Compiler.Backend.X86.Frame.String . StringFragment $ fmap (replaceRegister undefined . getField @"val") strings
