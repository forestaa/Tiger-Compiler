module Compiler.Frontend.Language.Tiger.IntegrationSpec (spec) where

import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Exception (FrontendException (fromFrontendException, toFrontendException), SomeFrontendException (SomeFrontendException))
import Compiler.Frontend.FrameMock
import Compiler.Frontend.Language.Tiger (Tiger (Tiger))
import Compiler.Frontend.Language.Tiger.Parser
import Compiler.Frontend.Language.Tiger.Semant
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level
import Compiler.Frontend.Language.Tiger.Semant.MarkEscape
import Compiler.Frontend.Language.Tiger.Semant.TypeCheck
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.Language.Tiger.TestUtils
import Compiler.Frontend.Lexer
import Compiler.Frontend.SrcLoc
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Control.Exception.Safe (Exception (toException), MonadCatch, MonadThrow, catch)
import Control.Monad (when)
import Control.Monad.Except
import Data.ByteString.Lazy qualified as B
import Data.Data (Proxy (Proxy))
import Data.Extensible
import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import Data.Maybe (isJust)
import GHC.Base (undefined)
import RIO hiding (catch)
import Test.Hspec

spec :: Spec
spec = do
  validTestSpec
  invalidTestSpec
  complexTestSpec

validTestSpec :: Spec
validTestSpec = describe "valid integration test for tiger to translate" $ do
  it "test01.tig" $ do
    let testcase = tigerTest "test01.tig"
    res <- translateTest' testcase
    let temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
    res.main
      `shouldBe` F.Proc
        { body =
            IR.Exp
              ( ( IR.Move
                    (IR.Temp temp1)
                    ( ( IR.Move
                          (IR.Temp temp0)
                          ( IR.Call
                              (IR.Name (U.Label "initArray" (U.Unique 11)))
                              [IR.Const 10, IR.Const 0]
                          )
                      )
                        `IR.ESeq` (IR.Temp temp0)
                    )
                )
                  `IR.ESeq` (IR.Temp temp1)
              ),
          frame =
            FrameMock
              { name = U.Label "tiger" (U.Unique 0),
                formals = [InFrame 0],
                numberOfLocals = 0
              }
        }
    res.fragments `shouldBe` []

  it "test02.tig" $ do
    let testcase = tigerTest "test02.tig"
    res <- translateTest' testcase
    let temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
    res.main
      `shouldBe` F.Proc
        { body =
            IR.Exp
              ( IR.Move
                  (IR.Temp temp1)
                  ( IR.Move
                      (IR.Temp temp0)
                      ( IR.Call
                          (IR.Name (U.Label "initArray" (U.Unique 11)))
                          [IR.Const 10, IR.Const 0]
                      )
                      IR.>>& IR.Temp temp0
                  )
                  IR.>>& IR.Temp temp1
              ),
          frame =
            FrameMock
              { name = U.Label "tiger" (U.Unique 0),
                formals = [InFrame 0],
                numberOfLocals = 0
              }
        }
    res.fragments `shouldBe` []

  it "test03.tig" $ do
    let testcase = tigerTest "test03.tig"
    res <- translateTest' testcase
    let temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        nobody = U.Label "L" (U.Unique 11)
        somebody = U.Label "L" (U.Unique 13)
    res.main
      `shouldBe` F.Proc
        { body =
            IR.Exp
              ( IR.Move
                  (IR.Temp temp1)
                  ( IR.Move
                      (IR.Temp temp0)
                      ( IR.Call
                          (IR.Name (U.Label "malloc" (U.Unique 12)))
                          [IR.Const 8]
                      )
                      IR.>> IR.Move
                        (IR.Mem (IR.BinOp IR.Plus (IR.Temp temp0) (IR.Const 0)))
                        (IR.Name nobody)
                      IR.>> IR.Move
                        (IR.Mem (IR.BinOp IR.Plus (IR.Temp temp0) (IR.Const 4)))
                        (IR.Const 1000)
                      IR.>>& IR.Temp temp0
                  )
                  IR.>>& IR.Move
                    (IR.Mem (IR.BinOp IR.Plus (IR.Temp temp1) (IR.Const 0)))
                    (IR.Name somebody)
                  IR.>>& IR.Temp temp1
              ),
          frame =
            FrameMock
              { name = U.Label "tiger" (U.Unique 0),
                formals = [InFrame 0],
                numberOfLocals = 0
              }
        }
    res.fragments
      `shouldBe` [ F.String nobody "\"Nobody\"",
                   F.String somebody "\"Somebody\""
                 ]

  it "test04.tig" $ do
    let testcase = tigerTest "test04.tig"
    res <- translateTest' testcase
    let temp0 = U.Temp "t" (U.Unique 0)
        temp1 = U.Temp "t" (U.Unique 1)
        fp = U.Temp "fp" (U.Unique 0)
        rv = U.Temp "rv" (U.Unique 0)
        label12 = U.Label "L" (U.Unique 12)
        label13 = U.Label "L" (U.Unique 13)
        label14 = U.Label "L" (U.Unique 14)
        nfactor = U.Label "nfactor" (U.Unique 11)
    res.main
      `shouldBe` F.Proc
        { body =
            IR.Exp (IR.Call (IR.Name nfactor) [IR.Temp fp, IR.Const 10]),
          frame =
            FrameMock
              { name = U.Label "tiger" (U.Unique 0),
                formals = [InFrame 0],
                numberOfLocals = 0
              }
        }
    res.fragments
      `shouldBe` [ F.Proc
                     { body =
                         IR.Move
                           (IR.Temp rv)
                           ( IR.CJump IR.Eq (IR.Temp temp0) (IR.Const 0) label12 label13
                               IR.>> IR.Label label12
                               IR.>> IR.Move (IR.Temp temp1) (IR.Const 1)
                               IR.>> IR.Jump (IR.Name label14) [label14]
                               IR.>> IR.Label label13
                               IR.>> IR.Move
                                 (IR.Temp temp1)
                                 ( IR.BinOp
                                     IR.Mul
                                     (IR.Temp temp0)
                                     ( IR.Call
                                         (IR.Name nfactor)
                                         [ IR.Mem (IR.BinOp IR.Plus (IR.Const 0) (IR.Temp fp)),
                                           IR.BinOp IR.Minus (IR.Temp temp0) (IR.Const 1)
                                         ]
                                     )
                                 )
                               IR.>> IR.Jump (IR.Name label14) [label14]
                               IR.>> IR.Label label14
                               IR.>>& (IR.Temp temp1)
                           ),
                       frame =
                         FrameMock
                           { name = U.Label "nfactor" (U.Unique 11),
                             formals = [InFrame 0, InReg temp0],
                             numberOfLocals = 0
                           }
                     }
                 ]

  it "valid test cases" $ do
    let tigerTests = (++) <$> (("test/Compiler/Frontend/Language/Tiger/samples/test" ++) <$> validTestCases) <*> [".tig"]
    res <- runExceptT (traverse (ExceptT . translateTest) tigerTests)
    res `shouldSatisfy` isRight
  where
    validTestCases = (\(d :: Integer) -> if d < 10 then '0' : show d else show d) <$> concat [[1 .. 8], [12], [27], [30], [37], [41 .. 42], [44], [46 .. 48]]

invalidTestSpec :: Spec
invalidTestSpec = describe "invalid integration test cases for tiger to translate" $ do
  it "then and else type differ" $
    runErrorTranslateTest (tigerTest "test09.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "body of while isTypeCheckErrorAnd not unit" $
    runErrorTranslateTest (tigerTest "test10.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedUnitType)

  it "hi in for is not int" $
    runErrorTranslateTest (tigerTest "test11.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "incompatible comparison: lt" $
    runErrorTranslateTest (tigerTest "test13.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "incompatible comparison: eq" $
    runErrorTranslateTest (tigerTest "test14.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "if-then returns non unit" $
    runErrorTranslateTest (tigerTest "test15.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedUnitType)

  it "invalid recursive type" $
    runErrorTranslateTest (tigerTest "test16.tig") (`shouldSatisfy` isTypeCheckErrorAndInvalidRecTypeDeclaration)

  it "invalid recursive type: interrupted" $
    runErrorTranslateTest (tigerTest "test17.tig") (`shouldSatisfy` isTypeCheckErrorAndUnknownType)

  it "invalid recursive function: interrupted" $
    runErrorTranslateTest (tigerTest "test18.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (tigerTest "test19.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "undefined variable" $
    runErrorTranslateTest (tigerTest "test20.tig") (`shouldSatisfy` isTypeCheckErrorAndUndefinedVariable)

  it "procedure returns value" $
    runErrorTranslateTest (tigerTest "test21.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "missing field in record" $
    runErrorTranslateTest (tigerTest "test22.tig") (`shouldSatisfy` isTypeCheckErrorAndMissingRecordField)

  it "type mismatch" $
    runErrorTranslateTest (tigerTest "test23.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "not array variable" $
    runErrorTranslateTest (tigerTest "test24.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedArrayType)

  it "not record variable" $
    runErrorTranslateTest (tigerTest "test25.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedRecordType)

  it "integer required" $
    runErrorTranslateTest (tigerTest "test26.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "different record type" $
    runErrorTranslateTest (tigerTest "test28.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "different array type" $
    runErrorTranslateTest (tigerTest "test29.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "init type differs from declared" $
    runErrorTranslateTest (tigerTest "test31.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "init type of array differed from declared" $
    runErrorTranslateTest (tigerTest "test32.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "unknown type" $
    runErrorTranslateTest (tigerTest "test33.tig") (`shouldSatisfy` isTypeCheckErrorAndUnknownType)

  it "type mismatched in function call" $
    runErrorTranslateTest (tigerTest "test34.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "less argument" $
    runErrorTranslateTest (tigerTest "test35.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "more argument" $
    runErrorTranslateTest (tigerTest "test36.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedTypes)

  it "type already declared" $
    runErrorTranslateTest (tigerTest "test38.tig") (`shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName)

  it "function already declared" $
    runErrorTranslateTest (tigerTest "test39.tig") (`shouldSatisfy` isTypeCheckErrorAndMultiDeclaredName)

  it "procedure returns value" $
    runErrorTranslateTest (tigerTest "test40.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedType)

  it "type mismatch in addition" $
    runErrorTranslateTest (tigerTest "test43.tig") (`shouldSatisfy` isTypeCheckErrorAndExpectedIntType)

  it "mismatch initialization by nil" $
    runErrorTranslateTest (tigerTest "test45.tig") (`shouldSatisfy` isTypeCheckErrorAndNotDeterminedNilType)

complexTestSpec :: Spec
complexTestSpec = describe "complex integration test for tiger to translate" $ do
  it "merge.tig" $ do
    let merge = tigerTest "merge.tig"
    res <- translateTest merge
    res `shouldSatisfy` isRight

  it "queens.tig" $ do
    let merge = tigerTest "queens.tig"
    res <- translateTest merge
    res `shouldSatisfy` isRight

translateTest' :: FilePath -> IO (F.ProgramFragments FrameMock)
translateTest' = translateTest >=> either throwM pure

translateTest :: FilePath -> IO (Either SomeFrontendException (F.ProgramFragments FrameMock))
translateTest file = runIODef . runEitherEff @"frontendException" . U.evalUniqueEff @"label" . U.evalUniqueEff @"temp" $ do
  bs <- liftEff (Proxy :: Proxy "IO") $ B.readFile file
  processFrontend @Tiger file bs

runErrorTranslateTest :: FilePath -> (SemantAnalysisError -> IO ()) -> IO ()
runErrorTranslateTest file assert = do
  (translateTest' file >> pure ())
    `frontendCatch` (\(ParserException msg) -> expectationFailure $ "parse error: " ++ msg)
    `frontendCatch` (\(L _ e) -> assert e)
  where
    frontendCatch :: (MonadThrow m, MonadCatch m, FrontendException e) => m a -> (e -> m a) -> m a
    frontendCatch m f =
      m `catch` \e@(SomeFrontendException _) -> case fromFrontendException e of
        Nothing -> throwM e
        Just e -> f e

tigerTest :: String -> FilePath
tigerTest file = "test/Compiler/Frontend/Language/Tiger/samples/" ++ file
