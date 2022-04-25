module Compiler.Backend.X86.IntegratoinTigerSpec where

import Compiler.Backend.X86.Arch (Assembly (Label), fromUniqueLabel)
import Compiler.Backend.X86.Codegen (codegen)
import Compiler.Backend.X86.Frame (Frame)
import Compiler.Backend.X86.Liveness qualified as L
import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Language.Tiger (Tiger (Tiger))
import Compiler.Intermediate.Canonical (processIntermediate)
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
integrationSpec = describe "hoge" $ do
  it "test01" $ do
    let testcase = tigerTest "test01.tig"
    result <- compileTest testcase
    result
      `shouldBe` [ [ L.Label {label = fromUniqueLabel (U.Label "L" (U.Unique 12)), val = Label (fromUniqueLabel (U.Label "L" (U.Unique 12)))}
                   ]
                 ]

compileTest :: FilePath -> IO ([[L.ControlFlow U.Temp (Assembly U.Temp)]])
compileTest file = (=<<) (either throwM pure) . runIODef . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" . runEitherEff @"exception" $ do
  bs <- liftEff (Proxy :: Proxy "IO") $ B.readFile file
  fragments <- mapLeftEff toException $ processFrontend @Tiger @Frame file bs
  mapM process fragments
  where
    process (F.Proc stm frame) = processIntermediate stm >>= codegen

tigerTest :: String -> FilePath
tigerTest file = "test/Compiler/Frontend/Language/Tiger/samples/" ++ file
