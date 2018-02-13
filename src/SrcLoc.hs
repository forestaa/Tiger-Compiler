module SrcLoc where


data SrcLoc = SrcLoc FilePath !Int !Int deriving (Show, Eq)
mkSrcLoc :: FilePath -> SrcLoc
mkSrcLoc file = SrcLoc file 1 1
advanceSrcLoc :: SrcLoc -> Char -> SrcLoc --TODO: handling tab
advanceSrcLoc (SrcLoc f row col) '\n' = SrcLoc f (row + 1) 1
advanceSrcLoc (SrcLoc f row col) _    = SrcLoc f row (col + 1)

data RealSrcSpan = RealSrcSpan {
        srcFile :: FilePath,
        srcSRow :: !Int,
        srcSCol :: !Int,
        srcERow :: !Int,
        srcECol :: !Int
      } deriving (Show, Eq)
mkRealSrcSpan :: SrcLoc -> Int -> RealSrcSpan
mkRealSrcSpan (SrcLoc file row col) len = RealSrcSpan file row col row (col + len)

data RealLocated e = L RealSrcSpan e deriving (Show, Eq)
