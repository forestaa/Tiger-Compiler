{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Frontend.Language.Tiger.LSyntax where

import Compiler.Frontend.AbstSyntax
import Compiler.Frontend.Id
import Compiler.Frontend.Language.Tiger.Syntax qualified as T
import Compiler.Frontend.SrcLoc

$(mkFAbstSyntaxes ''RealLocated 'dummyRealLocated [''T.Exp, ''T.Value, ''T.FieldAssign, ''T.Op, ''T.Dec, ''T.Field, ''T.Type])
