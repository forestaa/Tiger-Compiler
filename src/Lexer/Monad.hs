{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer.Monad where

import Data.Word (Word8)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as B (w2c)

import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import Control.Monad.State.Strict
import Control.Monad.Except

import SrcLoc


data AlexInput = AlexInput SrcLoc B.ByteString

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar is not implemented"

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput loc bs) = case B.uncons bs of
  Nothing        -> Nothing
  Just (b', bs') -> Just (b', AlexInput (advanceSrcLoc loc (B.w2c b')) bs')

data PState = PState {
        location :: SrcLoc,
        buffer :: B.ByteString,
        startcode :: !Int,

        commentDepth :: !Int
      } deriving (Show, Eq)

initPState :: FilePath -> B.ByteString -> PState
initPState file buffer = PState (mkSrcLoc file) buffer 0 0

-- lexer and parser monad
-- TODO: this should be rewritten by using newtype
newtype P a = P {unP :: Eff '[StateDef PState, EitherDef String] a} deriving (Functor, Applicative, Monad, MonadError String, MonadState PState)
type Action a = AlexInput -> Int -> P a

runP :: FilePath -> B.ByteString -> P a -> Either String a
runP file buf p = leaveEff . runEitherDef . evalStateEff (unP p) $ initPState file buf

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

getCommentDepth :: P Int
getCommentDepth = commentDepth <$> get

setCommentDepth :: Int -> P ()
setCommentDepth d = modify $ \s -> s{commentDepth = d}

modifyCommentDepth :: (Int -> Int) -> P ()
modifyCommentDepth f = modify $ \s@PState{commentDepth = d} -> s{commentDepth = f d}

andBegin :: Lexer.Monad.Action a -> Int -> Lexer.Monad.Action a
(action `andBegin` sc) inp len = setLexState sc >> action inp len