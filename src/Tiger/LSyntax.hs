{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Tiger.LSyntax where

import SrcLoc
import qualified Tiger.Syntax as S

import AbstSyntax.TH

$(deriveFAbstSyntaxes ''RealLocated [''S.Id, ''S.Exp, ''S.Value, ''S.FieldAssign, ''S.Op, ''S.Dec, ''S.Field, ''S.Type])