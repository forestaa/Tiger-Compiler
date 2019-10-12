module FrameMock where

import qualified Frame as F
import qualified IR
import qualified Unique as U

import Control.Monad.State.Strict
import Data.Extensible
import RIO hiding (exp)

newtype FrameMock =  FrameMock { unFrameMock :: Record '["name" :> U.Label, "formals" :> [AccessMock], "numberOfLocals" :> Int] } deriving (Show, Eq)
data AccessMock = InFrame Int | InReg U.Temp deriving (Show, Eq)
wordSize :: Int
wordSize = 4
allocateFormal :: (Lookup xs "temp" U.UniqueEff) => Bool -> StateT Int (Eff xs) AccessMock
allocateFormal False = InReg <$> lift U.newTemp
allocateFormal True = do
  modify (+1)
  gets $ InFrame . (* wordSize)
newFrame :: Lookup xs "temp" U.UniqueEff => U.Label -> [Bool] -> Eff xs FrameMock
newFrame name bs = do
  formals <- flip evalStateT 0 $ traverse allocateFormal bs
  pure . FrameMock $ #name @= name <: #formals @= formals <: #numberOfLocals @= 0 <: nil
name :: FrameMock -> U.Label
name (FrameMock r) = r ^. #name
formals :: FrameMock -> [AccessMock]
formals (FrameMock r) = r ^. #formals
allocLocal :: (Lookup xs "temp" U.UniqueEff) => FrameMock -> Bool -> Eff xs (FrameMock, AccessMock)
allocLocal frame False = (frame, ) . InReg <$> U.newTemp
allocLocal (FrameMock r) True = pure (FrameMock $ set #numberOfLocals numberOfLocals r, InFrame $ - numberOfLocals * wordSize)
  where
    numberOfLocals = r ^. #numberOfLocals + 1
exp :: AccessMock -> IR.Exp -> IR.Exp
exp (InFrame k) e = IR.Mem (IR.BinOp IR.Plus (IR.Const k) e)
exp (InReg t) _ = IR.Temp t
externalCall :: Lookup xs "label" U.UniqueEff => String -> [IR.Exp] -> Eff xs IR.Exp
externalCall s args = do
  label <- U.namedLabel s
  pure $ IR.Call (IR.Name label) args

instance F.Frame FrameMock where
  type Access FrameMock = AccessMock
  newFrame = newFrame
  name = name
  formals = formals
  allocLocal = allocLocal
  fp = U.Temp . U.Unique $ -1
  exp = exp
  wordSize = wordSize
  externalCall = externalCall

