module Main where

import Compiler.Backend (Backend (processBackend))
import Compiler.Backend.X86 ()
import Compiler.Backend.X86.Frame (Frame (Frame))
import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Language.Tiger (Tiger)
import Compiler.Intermediate.Unique qualified as U (evalUniqueEff)
import Control.Applicative ((<**>))
import Data.ByteString.Lazy qualified as B (readFile)
import Data.Extensible.Effect (liftEff, runEitherEff, throwEff)
import Data.Extensible.Effect.Default (runIODef)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, metavar, progDesc, strArgument)
import Path (addExtension, parseSomeFile, prjSomeBase, toFilePath)
import RIO

main :: IO ()
main = do
  args <- execParser parser
  file <- parseSomeFile args.input
  let filepath = prjSomeBase toFilePath file
  content <- B.readFile filepath
  newContent <- compile filepath content
  newFile <- prjSomeBase (fmap toFilePath . addExtension ".s") file
  writeFileUtf8Builder newFile newContent

data CommandLineArgument = CommandLineArgument {input :: FilePath}

parser :: ParserInfo CommandLineArgument
parser =
  info
    ((CommandLineArgument <$> strArgument (metavar "FILE")) <**> helper)
    (fullDesc <> progDesc "program description" <> header "helper description")

compile :: FilePath -> LByteString -> IO Utf8Builder
compile filepath content = (=<<) (either throwM pure) . runIODef . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" . runEitherEff @"exception" $ do
  fragments <- (=<<) (either (throwEff #exception . toException) pure) . runEitherEff @"frontendException" $ processFrontend @Tiger @Frame filepath content
  processBackend @Frame fragments
