{-# LANGUAGE TemplateHaskell #-}

module Linear.LSyntax where

import SrcLoc

import qualified Linear.Syntax as S
import AbstSyntax.TH

$(deriveFAbstSyntaxes ''RealLocated [''S.Id, ''S.Stm, ''S.Exp])