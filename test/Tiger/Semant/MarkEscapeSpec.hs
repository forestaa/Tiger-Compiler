module Tiger.Semant.MarkEscapeSpec where

import Test.Hspec
import RIO

import Tiger.Semant.MarkEscape
import Tiger.LSyntax (expToLExp)
import qualified Tiger.Syntax as T


spec :: Spec
spec =
  describe "markEscape test" $ do
    it "easy test: Nil" $ do
      let tiger = expToLExp T.Nil
      markEscape tiger `shouldBe` tiger
    it "easy test: For" $ do
      let tiger = expToLExp $ T.For "i" False (T.Int 0) (T.Int 0) (T.Nil)
      markEscape tiger `shouldBe` tiger
    it "nested function" $ do
      let tiger = expToLExp $ T.Let [T.FunDec "prettyprint" [T.Field "tree" False "tree"] (Just "String") (T.Let [T.VarDec "output" False Nothing (T.String ""), T.FunDec "write" [T.Field "s" False "String"] Nothing (T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")]))] (T.Seq [T.FunApply "show" [T.Int 0, T.Var (T.Id "tree")], T.Var (T.Id "output")]))] T.Nil
          expected = expToLExp $ T.Let [T.FunDec "prettyprint" [T.Field "tree" False "tree"] (Just "String") (T.Let [T.VarDec "output" True Nothing (T.String ""), T.FunDec "write" [T.Field "s" False "String"] Nothing (T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")]))] (T.Seq [T.FunApply "show" [T.Int 0, T.Var (T.Id "tree")], T.Var (T.Id "output")]))] T.Nil
      markEscape tiger `shouldBe` expected
    it "more nested function" $ do
      let tiger = expToLExp $ T.Let [T.FunDec "prettyprint" [T.Field "tree" False "tree"] (Just "String") (T.Let [T.VarDec "output" False Nothing (T.String ""), T.FunDec "write" [T.Field "s" False "String"] Nothing (T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")])), T.FunDec "show" [T.Field "n" False "Int", T.Field "t" False "tree"] Nothing (T.Let [T.FunDec "indent" [T.Field "s" False "String"] Nothing (T.For "i" False (T.Int 0) (T.Var (T.Id "n")) (T.Seq [T.FunApply "write" [T.String " "], T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")]), T.FunApply "write" [T.String "\n"]]))] T.Nil)] (T.Seq [T.FunApply "show" [T.Int 0, T.Var (T.Id "tree")], T.Var (T.Id "output")]))] T.Nil
          expected = expToLExp $ T.Let [T.FunDec "prettyprint" [T.Field "tree" False "tree"] (Just "String") (T.Let [T.VarDec "output" True Nothing (T.String ""), T.FunDec "write" [T.Field "s" False "String"] Nothing (T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")])), T.FunDec "show" [T.Field "n" True "Int", T.Field "t" False "tree"] Nothing (T.Let [T.FunDec "indent" [T.Field "s" False "String"] Nothing (T.For "i" False (T.Int 0) (T.Var (T.Id "n")) (T.Seq [T.FunApply "write" [T.String " "], T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")]), T.FunApply "write" [T.String "\n"]]))] T.Nil)] (T.Seq [T.FunApply "show" [T.Int 0, T.Var (T.Id "tree")], T.Var (T.Id "output")]))] T.Nil
      markEscape tiger `shouldBe` expected
    it "more and more nested function" $ do
      let tiger = expToLExp $ T.Let [T.FunDec "prettyprint" [T.Field "tree" False "tree"] (Just "String") (T.Let [T.VarDec "output" False Nothing (T.String ""), T.FunDec "write" [T.Field "s" False "String"] Nothing (T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")])), T.FunDec "show" [T.Field "n" False "Int", T.Field "t" False "tree"] Nothing (T.Let [T.FunDec "indent" [T.Field "s" False "String"] Nothing (T.For "i" False (T.Int 0) (T.Var (T.Id "n")) (T.Seq [T.FunApply "write" [T.String " "], T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")]), T.FunApply "write" [T.String "\n"]]))] (T.If (T.Op (T.Var (T.Id "t")) T.Eq T.Nil) (T.FunApply "indent" [T.String "."]) (Just (T.Seq [T.FunApply "indent" [T.Var (T.RecField (T.Id "t") "key")], T.FunApply "show" [T.Op (T.Var (T.Id "n")) T.Plus (T.Int 1), T.Var (T.RecField (T.Id "t") "left"), T.Var (T.RecField (T.Id "t") "right")]]))))] (T.Seq [T.FunApply "show" [T.Int 0, T.Var (T.Id "tree")], T.Var (T.Id "output")]))] T.Nil
          expected = expToLExp $ T.Let [T.FunDec "prettyprint" [T.Field "tree" False "tree"] (Just "String") (T.Let [T.VarDec "output" True Nothing (T.String ""), T.FunDec "write" [T.Field "s" False "String"] Nothing (T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")])), T.FunDec "show" [T.Field "n" True "Int", T.Field "t" False "tree"] Nothing (T.Let [T.FunDec "indent" [T.Field "s" False "String"] Nothing (T.For "i" False (T.Int 0) (T.Var (T.Id "n")) (T.Seq [T.FunApply "write" [T.String " "], T.Assign (T.Id "output") (T.FunApply "concat" [T.Var (T.Id "output"), T.Var (T.Id "s")]), T.FunApply "write" [T.String "\n"]]))] (T.If (T.Op (T.Var (T.Id "t")) T.Eq T.Nil) (T.FunApply "indent" [T.String "."]) (Just (T.Seq [T.FunApply "indent" [T.Var (T.RecField (T.Id "t") "key")], T.FunApply "show" [T.Op (T.Var (T.Id "n")) T.Plus (T.Int 1), T.Var (T.RecField (T.Id "t") "left"), T.Var (T.RecField (T.Id "t") "right")]]))))] (T.Seq [T.FunApply "show" [T.Int 0, T.Var (T.Id "tree")], T.Var (T.Id "output")]))] T.Nil
      markEscape tiger `shouldBe` expected
