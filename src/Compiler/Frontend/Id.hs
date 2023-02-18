module Compiler.Frontend.Id where

import Compiler.Frontend.SrcLoc
import RIO

type Id = Text

type LId = RealLocated Id

unLId :: LId -> Id
unLId (L _ id) = id

idToLId :: Id -> LId
idToLId = dummyRealLocated
