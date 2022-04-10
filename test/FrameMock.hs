module FrameMock where

import Control.Monad.State.Strict
import Data.Extensible
import Data.Extensible.Effect
import Frame qualified as F
import IR qualified
import RIO hiding (exp)
import Unique as U

data FrameMock = FrameMock {name :: Label, formals :: [AccessMock], numberOfLocals :: Int} deriving (Show, Eq)

data AccessMock = InFrame Int | InReg Temp deriving (Show, Eq)

wordSize :: Int
wordSize = 4

allocateFormal :: (Lookup xs "temp" U.UniqueEff) => Bool -> StateT Int (Eff xs) AccessMock
allocateFormal False = InReg <$> lift newTemp
allocateFormal True = do
  access <- gets $ InFrame . (* wordSize)
  modify (+ 1)
  pure access

newFrame :: Lookup xs "temp" UniqueEff => Label -> [Bool] -> Eff xs FrameMock
newFrame name bs = do
  formals <- flip evalStateT 0 $ traverse allocateFormal bs
  pure $ FrameMock name formals 0

name :: FrameMock -> Label
name FrameMock {name} = name

formals :: FrameMock -> [AccessMock]
formals FrameMock {formals} = formals

allocLocal :: (Lookup xs "temp" UniqueEff) => FrameMock -> Bool -> Eff xs (FrameMock, AccessMock)
allocLocal frame False = (frame,) . InReg <$> U.newTemp
allocLocal frame@FrameMock {} True = pure (frame {numberOfLocals = numberOfLocals}, InFrame $ -numberOfLocals * wordSize)
  where
    numberOfLocals = frame.numberOfLocals + 1

exp :: AccessMock -> IR.Exp -> IR.Exp
exp (InFrame k) e = IR.Mem (IR.BinOp IR.Plus (IR.Const k) e)
exp (InReg t) _ = IR.Temp t

externalCall :: Lookup xs "label" UniqueEff => String -> [IR.Exp] -> Eff xs IR.Exp
externalCall s args = do
  label <- U.namedLabel s
  pure $ IR.Call (IR.Name label) args

fp :: U.Temp
fp = newStringTemp "fp"

rv :: U.Temp
rv = newStringTemp "rv"

procEntryExit1 :: FrameMock -> IR.Stm -> IR.Stm
procEntryExit1 _ = id

instance F.Frame FrameMock where
  type Access FrameMock = AccessMock
  newFrame = newFrame
  name = name
  formals = formals
  allocLocal = allocLocal
  exp = exp
  wordSize = wordSize
  externalCall = externalCall
  fp = fp
  rv = rv
  procEntryExit1 = procEntryExit1
