{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Frontend.Language.Tiger.LSyntax where

import Compiler.Frontend.AbstSyntax
import Compiler.Frontend.Id
import Compiler.Frontend.SrcLoc
import Compiler.Frontend.Language.Tiger.Syntax qualified as T

$(mkFAbstSyntaxes ''RealLocated 'dummyRealLocated [''T.Exp, ''T.Value, ''T.FieldAssign, ''T.Op, ''T.Dec, ''T.Field, ''T.Type])
