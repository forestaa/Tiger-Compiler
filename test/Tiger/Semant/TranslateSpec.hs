module Tiger.Semant.TranslateSpec where

import Test.Hspec
import RIO
import Control.Monad.State.Strict
import Data.Extensible
import Data.Extensible.Effect.Default

import Tiger.Semant.Translate
import Frame
import qualified IR
import qualified Unique as U

spec :: Spec
spec = do
  getLevelExpSpec
  simpleVarExpSpec

getLevelExpSpec :: Spec
getLevelExpSpec = describe "getLevelExp test" $ do
  it "depth 1" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    level1 <- newLevel label1 []
    me <- pullInStaticLinksEff (level1 :: Level FrameMock)
    pure $ me `shouldBe` Just (IR.Temp (fp (Proxy :: Proxy FrameMock)))
  it "depth 2" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    level1 <- newLevel label1 []
    label2 <- U.newLabel
    level2 <- newLevel label2 []
    me <- pullInStaticLinksEff (level1 :: Level FrameMock)
    pure $ me `shouldBe` Just (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp (fp (Proxy :: Proxy FrameMock)))))
  it "with arguments" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    label2 <- U.newLabel
    label3 <- U.newLabel
    level1 <- newLevel label1 [True, False]
    level2 <- newLevel label2 [True, True]
    level3 <- newLevel label3 [False]
    me <- pullInStaticLinksEff (level1 :: Level FrameMock)
    pure $ me `shouldBe` Just (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp (fp (Proxy :: Proxy FrameMock)))))))
  it "with local variables" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    label2 <- U.newLabel
    level1 <- newLevel label1 []
    _ <- allocateLocalOnCurrentLevel True
    _ <- newLevel label2 []
    me <- pullInStaticLinksEff (level1 :: Level FrameMock)
    pure $ me `shouldBe` Just (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp (fp (Proxy :: Proxy FrameMock)))))


runUniqueLevelEff :: Eff '["temp" >: U.UniqueEff, "label" >: U.UniqueEff, "nestingLevel" >: NestingLevelEff f] a -> a
runUniqueLevelEff = leaveEff . runNestingLevelEff . U.runUniqueEff @"label" . U.runUniqueEff @"temp"


simpleVarExpSpec :: Spec
simpleVarExpSpec = describe "simpleVarExp test" $
  it "current level" . runUniqueLevelEff $ do
    label1 <- U.newLabel
    level1 <- newLevel label1 []
    allocateLocalOnCurrentLevel True >>= \case
      Nothing -> pure $ expectationFailure "something happens"
      Just access -> do
        me <- simpleVarExp (Access (level1 :: Level FrameMock) access)
        pure $ me `shouldBe` Just (Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-4)) (IR.Temp (fp (Proxy :: Proxy FrameMock))))))



newtype FrameMock =  FrameMock { unFrameMock :: Record '["name" :> U.Label, "formals" :> [AccessMock], "numberOfLocals" :> Int] }
data AccessMock = InFrame Int | InReg U.Temp
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
exp :: Proxy FrameMock -> AccessMock -> IR.Exp -> IR.Exp
exp _ (InFrame k) e = IR.Mem (IR.BinOp IR.Plus (IR.Const k) e)
exp _ (InReg t) _ = IR.Temp t

instance Frame FrameMock where
  type Access FrameMock = AccessMock
  newFrame = Tiger.Semant.TranslateSpec.newFrame
  name = Tiger.Semant.TranslateSpec.name
  formals = Tiger.Semant.TranslateSpec.formals
  allocLocal = Tiger.Semant.TranslateSpec.allocLocal
  fp _ = U.Temp $ U.Unique 100000
  exp = Tiger.Semant.TranslateSpec.exp

