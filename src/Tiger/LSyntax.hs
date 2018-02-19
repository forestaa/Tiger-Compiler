{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Tiger.LSyntax where

import SrcLoc
import Tiger.Syntax

import AbstSyntax.TH

$(deriveFAbstSyntaxes ''RealLocated [''Id, ''Exp, ''Value, ''FieldAssign, ''Op, ''Dec, ''Field, ''Type])
