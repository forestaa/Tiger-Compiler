{-# LANGUAGE DataKinds             #-}

module Lexer.Alex (
        AlexInput, alexInputPrevChar, alexGetByte,
        P, runP
       ) where

import Data.Word (Word8)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as B (w2c)

import           Data.Extensible.Effect
import           Data.Extensible.Effect.Default
import Control.Monad.State.Strict
import Control.Monad.Except

import SrcLoc


data AlexInput = AlexInput SrcLoc B.ByteString

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar is not implemented"

alexGetByte :: AlexInput -> Maybe (Char, AlexInput)
alexGetByte (AlexInput loc bs) = case B.uncons bs of
  Nothing        -> Nothing
  Just (b', bs') -> let c = B.w2c b'
                    in Just (c, AlexInput (advanceSrcLoc loc c) bs')


data PState = PState {
        location :: SrcLoc,
        buffer :: B.ByteString,
        startcode :: !Int
      }

initPState :: FilePath -> B.ByteString -> PState
initPState file buffer = PState (mkSrcLoc file) buffer 0

-- lexer and parser monad
-- TODO: this should be rewritten by using newtype
type P = Eff '[StateDef PState, EitherDef String]
type Action a = RealSrcSpan -> P a

runP :: FilePath -> B.ByteString -> P a -> Either String a
runP file buf p = leaveEff . runEitherDef . evalStateEff p $ initPState file buf

failP :: String -> P a
failP msg = throwError msg

getInput :: P AlexInput
getInput = do
  s <- get
  return $ AlexInput (location s) (buffer s)

setInput :: AlexInput -> P ()
setInput (AlexInput loc buf) = modify $ \s -> s{location = loc, buffer = buf}

getLexState :: P Int
getLexState = startcode <$> get

setLexState :: Int -> P ()
setLexState sc = modify $ \s -> s{startcode = sc}