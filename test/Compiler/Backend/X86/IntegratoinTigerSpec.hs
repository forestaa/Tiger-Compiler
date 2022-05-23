module Compiler.Backend.X86.IntegratoinTigerSpec where

import Compiler.Backend.X86.Arch
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.Frame (Frame)
import Compiler.Backend.X86.Liveness qualified as L
import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Language.Tiger (Tiger (Tiger))
import Compiler.Intermediate.Canonical (Canonical (Canonical))
import Compiler.Intermediate.Frame qualified as F (ProgramFragment (..))
import Compiler.Intermediate.Unique qualified as U
import Control.Exception.Safe (throwM)
import Data.ByteString.Lazy qualified as B
import Data.Either (either)
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff, liftEff, mapLeftEff, runEitherEff, throwEff)
import Data.Extensible.Effect.Default (runIODef)
import RIO hiding (throwM)
import Test.Hspec

spec :: Spec
spec = integrationSpec

integrationSpec :: Spec
integrationSpec = describe "integration spec for x86 backend of tiger" $ do
  it "test01.tig" $ do
    let testcase = tigerTest "test01.tig"
    result <- compileTest testcase
    let label0 = fromUniqueLabel $ U.Label "L" (U.Unique 0)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        temp2 = U.Temp "t" (U.Unique 2)
        temp3 = U.Temp "t" (U.Unique 3)
        tempRdi = U.Temp "RDI" (U.Unique 0)
        tempRsi = U.Temp "RSI" (U.Unique 0)
        tempRax = U.Temp "RAX" (U.Unique 0)
    result
      `shouldBe` [ [ L.Label {label = label0, val = Label label0},
                     L.Instruction {src = [], dst = [temp2], val = MovImmediate 10 temp2},
                     L.Instruction {src = [], dst = [temp3], val = MovImmediate 0 temp3},
                     L.Instruction {src = [temp2], dst = [tempRdi], val = MovRegister temp2 tempRdi},
                     L.Instruction {src = [temp3], dst = [tempRsi], val = MovRegister temp3 tempRsi},
                     L.Instruction {src = [temp2, temp3], dst = [tempRax], val = Call (fromUniqueLabel (U.Label "initArray" (U.Unique 11)))},
                     L.Instruction {src = [tempRax], dst = [temp0], val = MovRegister tempRax temp0},
                     L.Instruction {src = [temp0], dst = [temp1], val = MovRegister temp0 temp1},
                     L.Jump {jumps = [label12], val = Jump label12},
                     L.Label {label = label12, val = Label label12}
                   ]
                 ]

  it "test02.tig" $ do
    let testcase = tigerTest "test02.tig"
    result <- compileTest testcase
    let label0 = fromUniqueLabel $ U.Label "L" (U.Unique 0)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        temp2 = U.Temp "t" (U.Unique 2)
        temp3 = U.Temp "t" (U.Unique 3)
        tempRdi = U.Temp "RDI" (U.Unique 0)
        tempRsi = U.Temp "RSI" (U.Unique 0)
        tempRax = U.Temp "RAX" (U.Unique 0)
    result
      `shouldBe` [ [ L.Label {label = label0, val = Label label0},
                     L.Instruction {src = [], dst = [temp2], val = MovImmediate 10 temp2},
                     L.Instruction {src = [], dst = [temp3], val = MovImmediate 0 temp3},
                     L.Instruction {src = [temp2], dst = [tempRdi], val = MovRegister temp2 tempRdi},
                     L.Instruction {src = [temp3], dst = [tempRsi], val = MovRegister temp3 tempRsi},
                     L.Instruction {src = [temp2, temp3], dst = [tempRax], val = Call (fromUniqueLabel (U.Label "initArray" (U.Unique 11)))},
                     L.Instruction {src = [tempRax], dst = [temp0], val = MovRegister tempRax temp0},
                     L.Instruction {src = [temp0], dst = [temp1], val = MovRegister temp0 temp1},
                     L.Jump {jumps = [label12], val = Jump label12},
                     L.Label {label = label12, val = Label label12}
                   ]
                 ]

  it "test03.tig" $ do
    let testcase = tigerTest "test03.tig"
    result <- compileTest testcase
    let label0 = fromUniqueLabel $ U.Label "L" (U.Unique 0)
        label11 = fromUniqueLabel $ U.Label "L" (U.Unique 11)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        label13 = fromUniqueLabel $ U.Label "L" (U.Unique 13)
        label14 = fromUniqueLabel $ U.Label "L" (U.Unique 14)
        temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        temp2 = U.Temp "t" (U.Unique 2)
        temp3 = U.Temp "t" (U.Unique 3)
        temp4 = U.Temp "t" (U.Unique 4)
        temp5 = U.Temp "t" (U.Unique 5)
        tempRdi = U.Temp "RDI" (U.Unique 0)
        tempRax = U.Temp "RAX" (U.Unique 0)
        tempRip = U.Temp "RIP" (U.Unique 0)
    result
      `shouldBe` [ [ L.Label {label = label0, val = Label label0},
                     L.Instruction {src = [], dst = [temp2], val = MovImmediate 16 temp2},
                     L.Instruction {src = [temp2], dst = [tempRdi], val = MovRegister temp2 tempRdi},
                     L.Instruction {src = [temp2], dst = [tempRax], val = Call (fromUniqueLabel (U.Label "malloc" (U.Unique 12)))},
                     L.Instruction {src = [tempRax], dst = [temp0], val = MovRegister tempRax temp0},
                     L.Instruction {src = [], dst = [temp3], val = Lea label11 tempRip temp3},
                     L.Instruction {src = [temp0, temp3], dst = [], val = MovStoreIndirect temp3 0 temp0},
                     L.Instruction {src = [], dst = [temp4], val = MovImmediate 1000 temp4},
                     L.Instruction {src = [temp0, temp4], dst = [], val = MovStoreIndirect temp4 8 temp0},
                     L.Instruction {src = [temp0], dst = [temp1], val = MovRegister temp0 temp1},
                     L.Instruction {src = [], dst = [temp5], val = Lea label13 tempRip temp5},
                     L.Instruction {src = [temp1, temp5], dst = [], val = MovStoreIndirect temp5 0 temp1},
                     L.Jump {jumps = [label14], val = Jump label14},
                     L.Label {label = label14, val = Label label14}
                   ],
                   [ L.Instruction {src = [], dst = [], val = Text},
                     L.Instruction {src = [], dst = [], val = Global label13},
                     L.Instruction {src = [], dst = [], val = Data},
                     L.Instruction {src = [], dst = [], val = Align 16},
                     L.Instruction {src = [], dst = [], val = Type label13},
                     L.Instruction {src = [], dst = [], val = Size label13 24},
                     L.Label {label = label13, val = Label label13},
                     L.Instruction {src = [], dst = [], val = Long 10},
                     L.Instruction {src = [], dst = [], val = String "\"Somebody\""},
                     L.Instruction {src = [], dst = [], val = Zero 5}
                   ],
                   [ L.Instruction {src = [], dst = [], val = Text},
                     L.Instruction {src = [], dst = [], val = Global label11},
                     L.Instruction {src = [], dst = [], val = Data},
                     L.Instruction {src = [], dst = [], val = Align 16},
                     L.Instruction {src = [], dst = [], val = Type label11},
                     L.Instruction {src = [], dst = [], val = Size label11 24},
                     L.Label {label = label11, val = Label label11},
                     L.Instruction {src = [], dst = [], val = Long 8},
                     L.Instruction {src = [], dst = [], val = String "\"Nobody\""},
                     L.Instruction {src = [], dst = [], val = Zero 7}
                   ]
                 ]

  it "test04.tig" $ do
    let testcase = tigerTest "test04.tig"
    result <- compileTest testcase
    let label0 = fromUniqueLabel $ U.Label "L" (U.Unique 0)
        label12 = fromUniqueLabel $ U.Label "L" (U.Unique 12)
        label13 = fromUniqueLabel $ U.Label "L" (U.Unique 13)
        label14 = fromUniqueLabel $ U.Label "L" (U.Unique 14)
        label15 = fromUniqueLabel $ U.Label "L" (U.Unique 15)
        label16 = fromUniqueLabel $ U.Label "L" (U.Unique 16)
        label17 = fromUniqueLabel $ U.Label "L" (U.Unique 17)
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
        tempRdi = U.Temp "RDI" (U.Unique 0)
        tempRsi = U.Temp "RSI" (U.Unique 0)
        tempRax = U.Temp "RAX" (U.Unique 0)
        tempRip = U.Temp "RIP" (U.Unique 0)
        tempRbp = U.Temp "RBP" (U.Unique 0)
    result
      `shouldBe` [ [ L.Label {label = label0, val = Label label0},
                     L.Instruction {src = [tempRbp], dst = [temp2], val = MovRegister tempRbp temp2},
                     L.Instruction {src = [], dst = [temp3], val = MovImmediate 10 temp3},
                     L.Instruction {src = [temp2], dst = [tempRdi], val = MovRegister temp2 tempRdi},
                     L.Instruction {src = [temp3], dst = [tempRsi], val = MovRegister temp3 tempRsi},
                     L.Instruction {src = [temp2, temp3], dst = [tempRax], val = Call nfactor},
                     L.Jump {jumps = [label15], val = Jump label15},
                     L.Label {label = label15, val = Label label15}
                   ],
                   -- TODO: reverse this list order because the last label of main should be at the last, and use ret
                   [ L.Label {label = label12, val = Label label12},
                     L.Instruction {src = [], dst = [temp1], val = MovImmediate 1 temp1},
                     L.Label {label = label14, val = Label label14},
                     L.Instruction {src = [temp1], dst = [tempRax], val = MovRegister temp1 tempRax},
                     L.Jump {jumps = [label16], val = Jump label16},
                     L.Label {label = label13, val = Label label13},
                     L.Instruction {src = [temp0], dst = [temp6], val = MovRegister temp0 temp6},
                     L.Instruction {src = [tempRbp], dst = [temp7], val = MovLoadIndirect 0 tempRbp temp7},
                     L.Instruction {src = [temp7], dst = [temp4], val = MovRegister temp7 temp4},
                     L.Instruction {src = [temp0], dst = [temp8], val = MovRegister temp0 temp8},
                     L.Instruction {src = [temp8], dst = [temp8], val = SubImmediate 1 temp8},
                     L.Instruction {src = [temp4], dst = [tempRdi], val = MovRegister temp4 tempRdi},
                     L.Instruction {src = [temp8], dst = [tempRsi], val = MovRegister temp8 tempRsi},
                     L.Instruction {src = [temp4, temp8], dst = [tempRax], val = Call nfactor},
                     L.Instruction {src = [tempRax], dst = [temp5], val = MovRegister tempRax temp5},
                     L.Instruction {src = [temp6], dst = [temp9], val = MovRegister temp6 temp9},
                     L.Instruction {src = [temp9, temp5], dst = [temp9], val = MulRegister temp5 temp9},
                     L.Instruction {src = [temp9], dst = [temp1], val = MovRegister temp9 temp1},
                     L.Jump {jumps = [label14], val = Jump label14},
                     L.Label {label = nfactor, val = Label nfactor},
                     L.Instruction {src = [tempRbp, tempRdi], dst = [], val = MovStoreIndirect tempRdi 0 tempRbp},
                     L.Instruction {src = [tempRsi], dst = [temp0], val = MovRegister tempRsi temp0},
                     L.Instruction {src = [temp0], dst = [], val = CmpImmediate temp0 0},
                     L.CJump {jumps = [label12], val = JumpIfEqual label12},
                     L.Label {label = label17, val = Label label17},
                     L.Jump {jumps = [label13], val = Jump label13},
                     L.Label {label = label16, val = Label label16}
                   ]
                 ]

compileTest :: FilePath -> IO ([[L.ControlFlow U.Temp (Assembly U.Temp)]])
compileTest file = (=<<) (either throwM pure) . runIODef . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" . runEitherEff @"exception" $ do
  bs <- liftEff (Proxy :: Proxy "IO") $ B.readFile file
  fragments <- (=<<) (either (throwEff #exception . toException) pure) . runEitherEff @"frontendException" $ processFrontend @Tiger @Frame file bs
  mapM (codegen @Canonical) fragments

tigerTest :: String -> FilePath
tigerTest file = "test/Compiler/Frontend/Language/Tiger/samples/" ++ file
