{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Tiger.LSyntax where

import Id
import SrcLoc

import qualified Tiger.Syntax as S
import AbstSyntax.TH

$(mkFAbstSyntaxes ''RealLocated [''S.Exp, ''S.Value, ''S.FieldAssign, ''S.Op, ''S.Dec, ''S.Field, ''S.Type])