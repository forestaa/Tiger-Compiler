module Compiler.Frontend.SrcLoc
  ( advanceSrcLoc,
    dummyRealLocated,
    mkRealSrcSpan,
    mkSrcLoc,
    sL1,
    sL2,
    SrcLoc (..),
    unLoc,
    RealLocated (..),
    RealSrcSpan (..),
  )
where

import Compiler.Frontend.Exception (FrontendException (fromFrontendException, toFrontendException), SomeFrontendException, frontendExceptionFromException, frontendExceptionToException)
import Data.ByteString.Builder qualified as BB (stringUtf8)
import Data.Data (cast)
import RIO
import RIO.List (intersperse)
import RIO.Text qualified as T (unpack)

data SrcLoc = SrcLoc FilePath !Int !Int deriving (Show, Eq)

instance Display SrcLoc where
  display = displayShow

mkSrcLoc :: FilePath -> SrcLoc
mkSrcLoc file = SrcLoc file 1 1

advanceSrcLoc :: SrcLoc -> Char -> SrcLoc -- TODO: handling tab
advanceSrcLoc (SrcLoc f row _) '\n' = SrcLoc f (row + 1) 1
advanceSrcLoc (SrcLoc f row col) _ = SrcLoc f row (col + 1)

data RealSrcSpan = RealSrcSpan
  { srcFile :: FilePath,
    srcSRow :: !Int,
    srcSCol :: !Int,
    srcERow :: !Int,
    srcECol :: !Int
  }
  deriving (Eq)

instance Show RealSrcSpan where
  show = T.unpack . textDisplay

instance Display RealSrcSpan where
  display (RealSrcSpan f sr sc _ _) = mconcat $ intersperse ":" [Utf8Builder (BB.stringUtf8 f), display sr, display sc]

mkRealSrcSpan :: SrcLoc -> Int -> RealSrcSpan
mkRealSrcSpan (SrcLoc file row col) len = RealSrcSpan file row col row (col + len)

combineRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpan span1 span2 = RealSrcSpan file srow scol erow ecol
  where
    -- assume those srcFiles are the same
    file = span1.srcFile
    (srow, scol) = min (span1.srcSRow, span1.srcSCol) (span2.srcSRow, span2.srcSCol)
    (erow, ecol) = min (span1.srcERow, span1.srcECol) (span2.srcERow, span2.srcECol)

data RealLocated e = L RealSrcSpan e deriving (Eq, Functor)

instance Display e => Show (RealLocated e) where
  show = T.unpack . textDisplay

instance Display e => Display (RealLocated e) where
  display (L loc e) = locatedMessage loc $ display e

unLoc :: RealLocated a -> a
unLoc (L _ a) = a

sL1 :: RealLocated a -> b -> RealLocated b
sL1 (L span1 _) = L span1

sL2 :: RealLocated a -> RealLocated b -> c -> RealLocated c
sL2 (L span1 _) (L span2 _) = L (combineRealSrcSpan span1 span2)

locatedMessage :: RealSrcSpan -> Utf8Builder -> Utf8Builder
locatedMessage loc message = mconcat [display loc, ": ", message]

dummyRealLocated :: e -> RealLocated e
dummyRealLocated = L (mkRealSrcSpan (mkSrcLoc "dummy") 0)

instance FrontendException e => FrontendException (RealLocated e) where
  toFrontendException = realLocatedExceptionToException . fmap toFrontendException
  fromFrontendException = realLocatedExceptionFromException >=> \(L loc e') -> L loc <$> fromFrontendException e'

data SomeRealLocatedException = SomeRealLocaltedException (RealLocated SomeFrontendException)

instance Show SomeRealLocatedException where
  show = T.unpack . textDisplay

instance Display SomeRealLocatedException where
  display (SomeRealLocaltedException le) = display le

instance FrontendException SomeRealLocatedException where
  toFrontendException = frontendExceptionToException
  fromFrontendException = frontendExceptionFromException

realLocatedExceptionToException :: RealLocated SomeFrontendException -> SomeFrontendException
realLocatedExceptionToException = toFrontendException . SomeRealLocaltedException

realLocatedExceptionFromException :: SomeFrontendException -> Maybe (RealLocated SomeFrontendException)
realLocatedExceptionFromException x = do
  SomeRealLocaltedException lx <- fromFrontendException x
  cast lx
