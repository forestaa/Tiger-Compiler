{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Language.Tiger.LSyntax where

import Frontend.AbstSyntax
import Frontend.Id
import Frontend.SrcLoc
import Frontend.Language.Tiger.Syntax qualified as T

$(mkFAbstSyntaxes ''RealLocated 'dummyRealLocated [''T.Exp, ''T.Value, ''T.FieldAssign, ''T.Op, ''T.Dec, ''T.Field, ''T.Type])
