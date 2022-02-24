module SrcLoc
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

import RIO
import RIO.List

data SrcLoc = SrcLoc FilePath !Int !Int deriving (Show, Eq)

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
  show (RealSrcSpan f sr sc _ _) = intercalate ":" [f, show sr, show sc]

mkRealSrcSpan :: SrcLoc -> Int -> RealSrcSpan
mkRealSrcSpan (SrcLoc file row col) len = RealSrcSpan file row col row (col + len)

combineRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpan span1 span2 = RealSrcSpan file srow scol erow ecol
  where
    file = srcFile span1 -- assume those srcFiles are the same
    (srow, scol) = min (srcSRow span1, srcSCol span1) (srcSRow span2, srcSCol span2)
    (erow, ecol) = min (srcERow span1, srcECol span1) (srcERow span2, srcECol span2)

data RealLocated e = L RealSrcSpan e deriving (Eq, Functor)

instance Show e => Show (RealLocated e) where
  show (L loc e) = locatedMessage loc $ show e

unLoc :: RealLocated a -> a
unLoc (L _ a) = a

sL1 :: RealLocated a -> b -> RealLocated b
sL1 (L span1 _) = L span1

sL2 :: RealLocated a -> RealLocated b -> c -> RealLocated c
sL2 (L span1 _) (L span2 _) = L (combineRealSrcSpan span1 span2)

locatedMessage :: RealSrcSpan -> String -> String
locatedMessage loc message = concat [show loc, ": ", message]

dummyRealLocated :: e -> RealLocated e
dummyRealLocated = L (mkRealSrcSpan (mkSrcLoc "dummy") 0)
