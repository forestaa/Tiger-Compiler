{-# LANGUAGE TemplateHaskell #-}

module Linear.LSyntax where

import Env
import SrcLoc

import AbstSyntax.TH
import qualified Linear.Syntax as S

$(mkFAbstSyntaxes ''RealLocated [''S.Stm, ''S.Exp])