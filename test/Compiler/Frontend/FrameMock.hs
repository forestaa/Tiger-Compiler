module Compiler.Frontend.FrameMock where

import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique as U
import Control.Monad.State.Strict
import Data.Extensible
import Data.Extensible.Effect
import GHC.Records (HasField (..))
import RIO hiding (exp)

data FrameMock = FrameMock {name :: Label, formals :: [AccessMock], localVariables :: [AccessMock], head :: Int} deriving (Show, Eq)

instance HasField "numberOfLocals" FrameMock Int where
  getField frame = length frame.localVariables

data AccessMock = InFrame Int | InReg Temp deriving (Show, Eq)

isInRegister :: AccessMock -> Bool
isInRegister (InReg _) = True
isInRegister _ = False

isInFrame :: AccessMock -> Bool
isInFrame (InFrame _) = True
isInFrame _ = False

wordSize :: Int
wordSize = 4

allocateFormal :: (Lookup xs "temp" U.UniqueEff) => Bool -> StateT Int (Eff xs) AccessMock
allocateFormal False = InReg <$> lift newTemp
allocateFormal True = do
  access <- gets InFrame
  modify (flip (-) wordSize)
  pure access

newFrame :: Lookup xs "temp" UniqueEff => Label -> [Bool] -> Eff xs FrameMock
newFrame name bs = do
  (formals, head) <- flip runStateT 0 $ traverse allocateFormal bs
  pure $ FrameMock name formals [] head

name :: FrameMock -> Label
name FrameMock {name} = name

formals :: FrameMock -> [AccessMock]
formals FrameMock {formals} = formals

allocLocal :: (Lookup xs "temp" UniqueEff) => FrameMock -> Bool -> Eff xs (FrameMock, AccessMock)
allocLocal frame False = do
  access <- InReg <$> U.newTemp
  pure (frame {localVariables = frame.localVariables ++ [access]}, access)
allocLocal frame True = do
  let access = InFrame frame.head
  pure (frame {localVariables = frame.localVariables ++ [access], head = frame.head - wordSize}, access)

exp :: AccessMock -> IR.Exp -> IR.Exp
exp (InFrame k) e = IR.Mem (IR.BinOp IR.Plus (IR.Const k) e)
exp (InReg t) _ = IR.Temp t

externalCall :: Lookup xs "label" UniqueEff => Text -> [IR.Exp] -> Eff xs IR.Exp
externalCall s args = do
  let label = U.externalLabel s
  pure $ IR.Call (IR.Name label) args

fp :: U.Temp
fp = newUniqueTextTemp "fp"

rv :: U.Temp
rv = newUniqueTextTemp "rv"

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
