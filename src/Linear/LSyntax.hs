{-# LANGUAGE TemplateHaskell #-}

module Linear.LSyntax where

import AbstSyntax.TH
import Id
import Linear.Syntax qualified as S
import SrcLoc

$(mkFAbstSyntaxes ''RealLocated 'dummyRealLocated [''S.Stm, ''S.Exp])
