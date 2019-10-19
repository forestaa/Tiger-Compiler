module Id where

import RIO
import SrcLoc


type Id = String
type LId = RealLocated Id
unLId :: LId -> Id
unLId (L _ id) = id
idToLId :: Id -> LId 
idToLId = dummyRealLocated
