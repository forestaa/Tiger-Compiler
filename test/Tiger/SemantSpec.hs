module Tiger.SemantSpec (spec) where

import RIO
import Test.Hspec

import Tiger.Semant
import Tiger.Semant.Translate
import Tiger.Semant.Types
import qualified Tiger.LSyntax as T (expToLExp)
import qualified Tiger.Syntax as T
import qualified Env as E
import qualified Frame as F
import FrameMock
import qualified IR
import SrcLoc
import Unique

import Data.Extensible
import qualified RIO.List.Partial as Partial


spec :: Spec
spec = do
  translateIntSpec
  translateStringSpec
  translateNilSpec
  translateVariableSpec

-- translateSpec :: Spec
-- translateSpec = describe "translate test" $
--   it "translate simple variable: success" $ do
--     let typeEnv = E.empty
--         varEnv = E.empty
--         result = leaveEff . runTranslateEff @FrameMock $ do
--           label <- newLabel
--           level@(Level r) <- newLevel label [True]
--           let (access:_) = F.formals $ r ^. #frame
--           modifyEff #varEnv $ E.insert "x" (Var $ #type @= TInt <: #access @= Access (#level @= level <: #access @= access <: nil) <: nil)
--           translateExp . dummyRealLocated $ T.Var (dummyRealLocated (T.Id (dummyRealLocated "x")))
--     case result of
--       Left e -> expectationFailure $ show e
--       Right e -> e `shouldBe` (Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const 4) (IR.Temp (F.fp @FrameMock)))), TInt)

translateIntSpec :: Spec
translateIntSpec = describe "translate int test" $ do
  it "translate 0" $ do
    let ast = T.expToLExp $ T.Int 0
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Const 0)
        ty `shouldBe` TInt

translateStringSpec :: Spec
translateStringSpec = describe "translate string test" $ do
  it "translate 'hoge'" $ do
    let ast = T.expToLExp $ T.String "hoge"
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        exp `shouldSatisfy` expP
        ty `shouldBe` TString
        length fragments `shouldBe` 1
        Partial.head fragments `shouldSatisfy` fragmentP
    where
      expP (Ex (IR.Name _)) = True
      expP _ = False
      fragmentP (F.String (Label _ _) s) = s == "hoge"
      fragmentP _ = False

translateNilSpec :: Spec
translateNilSpec = describe "translate nil test" $ do
  it "translate nil" $ do
    let ast = T.expToLExp $ T.Nil
    case leaveEff $ runTranslateEff (translateExp @FrameMock ast) of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), fragments) -> do
        ty `shouldBe` TNil

translateVariableSpec :: Spec
translateVariableSpec = describe "translate variable test" $ do
  it "first local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          _ <- allocateLocalVariable "y" True TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-2*F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "second local variable, first is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "y")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" False TInt
          _ <- allocateLocalVariable "y" True TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "first local variable, second is not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" True TInt
          _ <- allocateLocalVariable "y" False TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldBe` Ex (IR.Mem (IR.BinOp IR.Plus (IR.Const (-F.wordSize @FrameMock)) (IR.Temp (F.fp @FrameMock))))
        ty `shouldBe` TInt

  it "local variable, not escaped" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          _ <- allocateLocalVariable "x" False TInt
          translateExp @FrameMock ast
    case result of
      Left e -> expectationFailure $ show e
      Right ((exp, ty), _) -> do
        exp `shouldSatisfy` inRegister
        ty `shouldBe` TInt
        where
          inRegister (Ex (IR.Temp _)) = True
          inRegister _ = False

  it "undefined variable" $ do
    let ast = T.expToLExp $ T.Var (T.Id "x")
        result = leaveEff . runTranslateEffWithNewLevel $ do
          translateExp @FrameMock ast
    case result of
      Right ret -> expectationFailure $ "should return undefined variable error: " ++ show ret
      Left (L _ e) -> e `shouldSatisfy` isUndefinedVariable
        where
          isUndefinedVariable (VariableUndefined id) = id == "x"
          isUndefinedVariable _ = False





-- spec =
--   describe "typing test" $ do
--     it "then and else type differ" . runErrorTypingTest $ testcase "test09.tig"
--     it "body of while is not unit" . runErrorTypingTest $ testcase "test10.tig"
--     it "hi in for is not int" . runErrorTypingTest $ testcase "test11.tig"
--     it "incompatible comparison: lt" . runErrorTypingTest $ testcase "test13.tig"
--     it "incompatible comparison: eq" . runErrorTypingTest $ testcase "test14.tig"
--     it "if-then returns non unit" . runErrorTypingTest $ testcase "test15.tig"
--     it "invalid recursion type " . runErrorTypingTest $ testcase "test16.tig"
--     it "undefined variable" . runErrorTypingTest $ testcase "test19.tig"
--     it "undefined variable" . runErrorTypingTest $ testcase "test20.tig"
--     it "procedure returns value" . runErrorTypingTest $ testcase "test21.tig"
--     it "missing field in record" . runErrorTypingTest $ testcase "test22.tig"
--     it "type mismatch" . runErrorTypingTest $ testcase "test23.tig"
--     it "not array variable" . runErrorTypingTest $ testcase "test24.tig"
--     it "not record variable" . runErrorTypingTest $ testcase "test25.tig"
--     it "integer required" . runErrorTypingTest $ testcase "test26.tig"
--     it "different record type" . runErrorTypingTest $ testcase "test28.tig"
--     it "different array type" . runErrorTypingTest $ testcase "test29.tig"
--     it "init type differs from declared" . runErrorTypingTest $ testcase "test31.tig"
--     it "init type of array differed from declared" . runErrorTypingTest $ testcase "test32.tig"
--     it "unknown type" . runErrorTypingTest $ testcase "test33.tig"
--     it "type mismatched in function call" . runErrorTypingTest $ testcase "test34.tig"
--     it "less argument" . runErrorTypingTest $ testcase "test35.tig"
--     it "more argument" . runErrorTypingTest $ testcase "test36.tig"
--     it "type already declared" . runErrorTypingTest $ testcase "test38.tig"
--     it "function already declared" . runErrorTypingTest $ testcase "test39.tig"
--     it "procedure returns value" . runErrorTypingTest $ testcase "test40.tig"
--     it "type mismatch in addition" . runErrorTypingTest $ testcase "test43.tig"
--     it "mismatch initialization by nil" . runErrorTypingTest $ testcase "test45.tig"
--     it "type already declared" . runErrorTypingTest $ testcase "test47.tig"
--     it "function already declared" . runErrorTypingTest $ testcase "test48.tig"
--     it "valid test" $ do
--       let testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> valid) <*> [".tig"]
--       res <- runExceptT (traverse typingTest testcases)
--       res `shouldSatisfy` isRight
--     it "merge.tig" $ do
--       let merge = "test/Tiger/samples/merge.tig"
--       res <- runExceptT (typingTest merge)
--       res `shouldSatisfy` isRight
--     it "queens.tig" $ do
--       let merge = "test/Tiger/samples/queens.tig"
--       res <- runExceptT (typingTest merge)
--       res `shouldSatisfy` isRight
--   where
--     testcase s = "test/Tiger/samples/" ++ s
--     valid = (\(d :: Integer) -> if d < 10 then '0' : show d else show d) <$> concat [[1..8], [12], [17..18], [27], [30], [37], [41..42], [44], [46]]

-- typingTest :: FilePath -> ExceptT String IO Type
-- typingTest file = do
--   bs <- liftIO $ B.readFile file
--   e <- liftEither $ runP parser file bs
--   liftEither . mapLeft show . runTyping $ typingExp e

-- runErrorTypingTest :: FilePath -> Expectation
-- runErrorTypingTest file = do
--   res <- runExceptT (typingTest file)
--   res `shouldSatisfy` isLeft
