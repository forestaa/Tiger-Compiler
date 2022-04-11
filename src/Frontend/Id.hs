module Frontend.Id where

import Frontend.SrcLoc
import RIO

type Id = String

type LId = RealLocated Id

unLId :: LId -> Id
unLId (L _ id) = id

idToLId :: Id -> LId
idToLId = dummyRealLocated
