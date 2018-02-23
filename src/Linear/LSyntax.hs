{-# LANGUAGE TemplateHaskell #-}

module Linear.LSyntax where

import SrcLoc

import qualified Linear.Syntax as S
import AbstSyntax.TH

$(mkFAbstSyntaxes ''RealLocated [''S.Id, ''S.Stm, ''S.Exp])