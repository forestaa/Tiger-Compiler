{-# LANGUAGE FieldSelectors #-}

module Lexer.Monad where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.ByteString.Internal qualified as B (w2c)
import Data.ByteString.Lazy qualified as B
import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import RIO
import SrcLoc

data AlexInput = AlexInput SrcLoc B.ByteString

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar is not implemented"

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput loc bs) = case B.uncons bs of
  Nothing -> Nothing
  Just (b', bs') -> Just (b', AlexInput (advanceSrcLoc loc (B.w2c b')) bs')

data PState = PState
  { location :: SrcLoc,
    buffer :: B.ByteString,
    startcode :: !Int,
    commentDepth :: !Int
  }
  deriving (Show, Eq)

initPState :: FilePath -> B.ByteString -> PState
initPState file buf = PState (mkSrcLoc file) buf 0 0

-- lexer and parser monad
newtype P a = P {unP :: Eff '[StateDef PState, EitherDef String] a} deriving (Functor, Applicative, Monad, MonadError String, MonadState PState)

type Action a = AlexInput -> Int -> P a

runP :: P a -> FilePath -> B.ByteString -> Either String a
runP p file bs = leaveEff . runEitherDef . evalStateEff (unP p) $ initPState file bs

failP :: String -> P a
failP = throwError

getInput :: P AlexInput
getInput = do
  s <- get
  pure $ AlexInput s.location s.buffer

setInput :: AlexInput -> P ()
setInput (AlexInput loc buf) = modify $ \s -> s {location = loc, buffer = buf}

getLexState :: P Int
getLexState = startcode <$> get

setLexState :: Int -> P ()
setLexState sc = modify $ \s -> s {startcode = sc}

getCommentDepth :: P Int
getCommentDepth = commentDepth <$> get

setCommentDepth :: Int -> P ()
setCommentDepth d = modify $ \s -> s {commentDepth = d}

modifyCommentDepth :: (Int -> Int) -> P ()
modifyCommentDepth f = modify $ \s@PState {commentDepth = d} -> s {commentDepth = f d}

andBegin :: Lexer.Monad.Action a -> Int -> Lexer.Monad.Action a
(action `andBegin` sc) inp len = setLexState sc >> action inp len
