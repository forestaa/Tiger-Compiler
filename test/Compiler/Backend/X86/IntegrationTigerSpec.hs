module Compiler.Backend.X86.IntegrationTigerSpec (spec) where

import Compiler.Backend.X86.Arch
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.Frame (Frame, ProcedureX86 (..), ProgramFragmentX86 (..), StringFragmentX86 (..), r8, r9, rax, rbp, rcx, rdi, rdx, rip, rsi, rsp)
import Compiler.Backend.X86.Liveness qualified as L
import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Language.Tiger (Tiger (Tiger))
import Compiler.Frontend.Language.Tiger.Samples (tigerTest)
import Compiler.Intermediate.Canonical (Canonical (Canonical))
import Compiler.Intermediate.Frame qualified as F (ProgramFragment (..))
import Compiler.Intermediate.Unique qualified as U
import Compiler.Utils.Maybe
import Control.Exception.Safe (throwM)
import Data.ByteString.Lazy qualified as B
import Data.Either (either)
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff, liftEff, mapLeftEff, runEitherEff, throwEff)
import Data.Extensible.Effect.Default (runIODef)
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
        temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        temp2 = U.Temp "t" (U.Unique 2)
        temp3 = U.Temp "t" (U.Unique 3)
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ L.Label {label' = mainLabel, val = Label mainLabel},
          L.Instruction {src = [], dst = [temp2], val = MovImmediate 10 temp2},
          L.Instruction {src = [], dst = [temp3], val = MovImmediate 0 temp3},
          L.Instruction {src = [temp2], dst = [rdi], val = MovRegister temp2 rdi},
          L.Instruction {src = [temp3], dst = [rsi], val = MovRegister temp3 rsi},
          L.Instruction {src = [temp2, temp3], dst = [rax], val = Call (fromUniqueLabel (U.Label "initArray" (U.Unique 11)))},
          L.Instruction {src = [rax], dst = [temp0], val = MovRegister rax temp0},
          L.Instruction {src = [temp0], dst = [temp1], val = MovRegister temp0 temp1},
          L.Jump {jumps = [label13], val = Jump label13},
          L.Label {label' = label13, val = Label label13}
        ]

  it "test02.tig" $ do
    let testcase = tigerTest "test02.tig"
    result <- compileTest testcase
    let mainLabel = fromUniqueLabel $ U.Label "tigerMain" (U.Unique 12)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        label13 = fromUniqueLabel $ U.Label "L" (U.Unique 13)
        temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        temp2 = U.Temp "t" (U.Unique 2)
        temp3 = U.Temp "t" (U.Unique 3)
    length result `shouldBe` 1
    (result !! 0).procedure.body
      `shouldBe` Just
        [ L.Label {label' = mainLabel, val = Label mainLabel},
          L.Instruction {src = [], dst = [temp2], val = MovImmediate 10 temp2},
          L.Instruction {src = [], dst = [temp3], val = MovImmediate 0 temp3},
          L.Instruction {src = [temp2], dst = [rdi], val = MovRegister temp2 rdi},
          L.Instruction {src = [temp3], dst = [rsi], val = MovRegister temp3 rsi},
          L.Instruction {src = [temp2, temp3], dst = [rax], val = Call (fromUniqueLabel (U.Label "initArray" (U.Unique 11)))},
          L.Instruction {src = [rax], dst = [temp0], val = MovRegister rax temp0},
          L.Instruction {src = [temp0], dst = [temp1], val = MovRegister temp0 temp1},
          L.Jump {jumps = [label13], val = Jump label13},
          L.Label {label' = label13, val = Label label13}
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
        temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        temp2 = U.Temp "t" (U.Unique 2)
        temp3 = U.Temp "t" (U.Unique 3)
        temp4 = U.Temp "t" (U.Unique 4)
        temp5 = U.Temp "t" (U.Unique 5)
    length result `shouldBe` 3
    (result !! 0).string.body
      `shouldBe` Just
        [ L.Instruction {src = [], dst = [], val = Text},
          L.Instruction {src = [], dst = [], val = Global label11},
          L.Instruction {src = [], dst = [], val = Data},
          L.Instruction {src = [], dst = [], val = Align 16},
          L.Instruction {src = [], dst = [], val = Type label11},
          L.Instruction {src = [], dst = [], val = Size label11 24},
          L.Label {label' = label11, val = Label label11},
          L.Instruction {src = [], dst = [], val = Long 8},
          L.Instruction {src = [], dst = [], val = Compiler.Backend.X86.Arch.String "\"Nobody\""},
          L.Instruction {src = [], dst = [], val = Zero 7}
        ]
    (result !! 1).string.body
      `shouldBe` Just
        [ L.Instruction {src = [], dst = [], val = Text},
          L.Instruction {src = [], dst = [], val = Global label13},
          L.Instruction {src = [], dst = [], val = Data},
          L.Instruction {src = [], dst = [], val = Align 16},
          L.Instruction {src = [], dst = [], val = Type label13},
          L.Instruction {src = [], dst = [], val = Size label13 24},
          L.Label {label' = label13, val = Label label13},
          L.Instruction {src = [], dst = [], val = Long 10},
          L.Instruction {src = [], dst = [], val = Compiler.Backend.X86.Arch.String "\"Somebody\""},
          L.Instruction {src = [], dst = [], val = Zero 5}
        ]
    (result !! 2).procedure.body
      `shouldBe` Just
        [ L.Label {label' = mainLabel, val = Label mainLabel},
          L.Instruction {src = [], dst = [temp2], val = MovImmediate 16 temp2},
          L.Instruction {src = [temp2], dst = [rdi], val = MovRegister temp2 rdi},
          L.Instruction {src = [temp2], dst = [rax], val = Call (fromUniqueLabel (U.Label "malloc" (U.Unique 12)))},
          L.Instruction {src = [rax], dst = [temp0], val = MovRegister rax temp0},
          L.Instruction {src = [], dst = [temp3], val = Lea label11 rip temp3},
          L.Instruction {src = [temp0, temp3], dst = [], val = MovStoreIndirect temp3 0 temp0},
          L.Instruction {src = [], dst = [temp4], val = MovImmediate 1000 temp4},
          L.Instruction {src = [temp0, temp4], dst = [], val = MovStoreIndirect temp4 8 temp0},
          L.Instruction {src = [temp0], dst = [temp1], val = MovRegister temp0 temp1},
          L.Instruction {src = [], dst = [temp5], val = Lea label13 rip temp5},
          L.Instruction {src = [temp1, temp5], dst = [], val = MovStoreIndirect temp5 0 temp1},
          L.Jump {jumps = [label15], val = Jump label15},
          L.Label {label' = label15, val = Label label15}
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
        temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        temp2 = U.Temp "t" (U.Unique 2)
        temp3 = U.Temp "t" (U.Unique 3)
        temp4 = U.Temp "t" (U.Unique 4)
        temp5 = U.Temp "t" (U.Unique 5)
        temp6 = U.Temp "t" (U.Unique 6)
        temp7 = U.Temp "t" (U.Unique 7)
        temp8 = U.Temp "t" (U.Unique 8)
        temp9 = U.Temp "t" (U.Unique 9)
    length result `shouldBe` 2
    (result !! 0).procedure.body
      `shouldBe` Just
        [ L.Label {label' = label12, val = Label label12},
          L.Instruction {src = [], dst = [temp1], val = MovImmediate 1 temp1},
          L.Label {label' = label14, val = Label label14},
          L.Instruction {src = [temp1], dst = [rax], val = MovRegister temp1 rax},
          L.Jump {jumps = [label15], val = Jump label15},
          L.Label {label' = label13, val = Label label13},
          L.Instruction {src = [temp0], dst = [temp4], val = MovRegister temp0 temp4},
          L.Instruction {src = [rbp], dst = [temp5], val = MovLoadIndirect 0 rbp temp5},
          L.Instruction {src = [temp5], dst = [temp2], val = MovRegister temp5 temp2},
          L.Instruction {src = [temp0], dst = [temp6], val = MovRegister temp0 temp6},
          L.Instruction {src = [temp6], dst = [temp6], val = SubImmediate 1 temp6},
          L.Instruction {src = [temp2], dst = [rdi], val = MovRegister temp2 rdi},
          L.Instruction {src = [temp6], dst = [rsi], val = MovRegister temp6 rsi},
          L.Instruction {src = [temp2, temp6], dst = [rax], val = Call nfactor},
          L.Instruction {src = [rax], dst = [temp3], val = MovRegister rax temp3},
          L.Instruction {src = [temp4], dst = [temp7], val = MovRegister temp4 temp7},
          L.Instruction {src = [temp7, temp3], dst = [temp7], val = MulRegister temp3 temp7},
          L.Instruction {src = [temp7], dst = [temp1], val = MovRegister temp7 temp1},
          L.Jump {jumps = [label14], val = Jump label14},
          L.Label {label' = nfactor, val = Label nfactor},
          L.Instruction {src = [rbp, rdi], dst = [], val = MovStoreIndirect rdi 0 rbp},
          L.Instruction {src = [rsi], dst = [temp0], val = MovRegister rsi temp0},
          L.Instruction {src = [temp0], dst = [], val = CmpImmediate temp0 0},
          L.CJump {jumps = [label12], val = JumpIfEqual label12},
          L.Label {label' = label16, val = Label label16},
          L.Jump {jumps = [label13], val = Jump label13},
          L.Label {label' = label15, val = Label label15}
        ]
    (result !! 1).procedure.body
      `shouldBe` Just
        [ L.Label {label' = mainLabel, val = Label mainLabel},
          L.Instruction {src = [rbp], dst = [temp8], val = MovRegister rbp temp8},
          L.Instruction {src = [], dst = [temp9], val = MovImmediate 10 temp9},
          L.Instruction {src = [temp8], dst = [rdi], val = MovRegister temp8 rdi},
          L.Instruction {src = [temp9], dst = [rsi], val = MovRegister temp9 rsi},
          L.Instruction {src = [temp8, temp9], dst = [rax], val = Call nfactor},
          L.Jump {jumps = [label18], val = Jump label18},
          L.Label {label' = label18, val = Label label18}
        ]

compileTest :: FilePath -> IO ([ProgramFragmentX86 [L.ControlFlow U.Temp (Assembly U.Temp)]])
compileTest file = (=<<) (either throwM pure) . runIODef . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" . runEitherEff @"exception" $ do
  bs <- liftEff (Proxy @"IO") $ B.readFile file
  fragments <- (=<<) (either (throwEff #exception . toException) pure) . runEitherEff @"frontendException" $ processFrontend @Tiger @Frame file bs
  codegen @Canonical fragments
