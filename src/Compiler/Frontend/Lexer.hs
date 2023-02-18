{-# LANGUAGE FieldSelectors #-}

module Compiler.Frontend.Lexer where

import Compiler.Frontend.Exception (FrontendException, fromFrontendException, frontendExceptionFromException, frontendExceptionToException, toFrontendException)
import Compiler.Frontend.SrcLoc
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.ByteString.Internal qualified as B (w2c)
import Data.ByteString.Lazy qualified as B
import Data.Extensible.Effect (Eff, evalStateEff, leaveEff)
import Data.Extensible.Effect.Default (EitherDef, StateDef, runEitherDef)
import RIO
import RIO.Text qualified as T (unpack)

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
  deriving (Eq)

initPState :: FilePath -> B.ByteString -> PState
initPState file buf = PState (mkSrcLoc file) buf 0 0

-- lexer and parser monad
newtype P a = P {unP :: Eff '[StateDef PState, EitherDef Text] a} deriving (Functor, Applicative, Monad, MonadError Text, MonadState PState)

type Action a = AlexInput -> Int -> P a

newtype ParserException = ParserException Text

instance Display ParserException where
  textDisplay (ParserException text) = "failed to parse: " <> text

instance Show ParserException where
  show = T.unpack . textDisplay

instance FrontendException ParserException where
  toFrontendException = frontendExceptionToException
  fromFrontendException = frontendExceptionFromException

runP :: P a -> FilePath -> B.ByteString -> Either ParserException a
runP p file bs = mapLeft ParserException . leaveEff . runEitherDef . evalStateEff (unP p) $ initPState file bs

failP :: Text -> P a
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

andBegin :: Action a -> Int -> Action a
(action `andBegin` sc) inp len = setLexState sc >> action inp len
