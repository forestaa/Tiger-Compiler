{-# LANGUAGE TemplateHaskell #-}

module Frontend.Language.Linear.LSyntax where

import Frontend.AbstSyntax
import Frontend.Id
import Frontend.Language.Linear.Syntax qualified as S
import Frontend.SrcLoc

$(mkFAbstSyntaxes ''RealLocated 'dummyRealLocated [''S.Stm, ''S.Exp])
