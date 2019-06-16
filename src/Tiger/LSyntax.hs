{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Tiger.LSyntax where

import Id
import SrcLoc

import qualified Tiger.Syntax as T
import AbstSyntax.TH

$(mkFAbstSyntaxes ''RealLocated 'dummyRealLocated [''T.Exp, ''T.Value, ''T.FieldAssign, ''T.Op, ''T.Dec, ''T.Field, ''T.Type])
