{-# LANGUAGE TemplateHaskell #-}

module Compiler.Frontend.Language.Linear.LSyntax where

import Compiler.Frontend.AbstSyntax
import Compiler.Frontend.Id
import Compiler.Frontend.Language.Linear.Syntax qualified as S
import Compiler.Frontend.SrcLoc

$(mkFAbstSyntaxes ''RealLocated 'dummyRealLocated [''S.Stm, ''S.Exp])
