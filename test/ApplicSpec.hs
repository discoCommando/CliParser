{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ApplicSpec where

import Applic
import Common2 (runParser, symbol)
import Data.Functor (($>))
import Data.Text
import GHC.Base (Alternative ((<|>)))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Text.Megaparsec as Mega

spec = do
  describe "applic" $ do
    describe "prefix" $ do
      let p = "test"
          t = prefix p ()

      it "parser" $ do
        runParser (parser t) p `shouldBe` Right ()
      it "prefix" $ do
        prefixes t `shouldBe` [p]
      describe "completion" $ do
        it "empty word" $ do
          runParser (completion t) "" `shouldBe` Right [p]
        it "non empty word" $ do
          runParser (completion t) "aaaa" `shouldBe` Right [p]

    describe "token" $ do
      let p = "dupa"
          t = token (symbol p >> pure ()) ["aaa", "bbb"]

      it "parser" $ do
        runParser (parser t) p `shouldBe` Right ()
      it "prefix" $ do
        prefixes t `shouldBe` []
      it "completion" $ do
        runParser (completion t) "" `shouldBe` Right ["aaa", "bbb"]

    describe "completionsHelper" $ do
      it "empty" $ do
        completionsHelper "" `shouldBe` ""
      it "first word" $
        completionsHelper "a" `shouldBe` ""
      it "first word with space" $
        completionsHelper "a " `shouldBe` "a"
      it "two wordsr" $
        completionsHelper "a a" `shouldBe` "a"

    describe "recover" $ do
      it "happy path" $ do
        runParser (recover (symbol "aaa" $> ["bbb"])) "aaa" `shouldBe` Right ["bbb"]
      it "not happy path" $ do
        runParser (recover (symbol "aaa" $> ["bbb"])) "ccc" `shouldBe` Right []
      it "happy path does consumes an input" $ do
        runParser (recover (symbol "aaa" $> ["bbb"]) *> Mega.takeRest) "aaa ccc" `shouldBe` Right "ccc" -- not sure if that's correct though
      it "not happy path does not consume any input" $ do
        runParser (recover (symbol "aaa" $> ["bbb"]) *> Mega.takeRest) "xxx ccc" `shouldBe` Right "xxx ccc"

    describe "applicative" $ do
      let pt = prefix "something" (,) <*> token (symbol "aaa" >> pure ()) ["bbb"] <*> token (symbol "ccc" >> pure ()) ["ddd"]
      it "parser" $ do
        runParser (parser pt) "something aaa ccc" `shouldBe` Right ((), ())
      it "prefix" $ do
        prefixes pt `shouldBe` ["something"]
      describe "completion" $ do
        it "empty word" $ do
          completions pt "" `shouldBe` ["something"]
        it "substring of the first word" $ do
          completions pt "somethin" `shouldBe` ["something"]
        it "whole first word" $ do
          completions pt "something" `shouldBe` ["something"]
        it "first word complete with space" $ do
          completions pt "something " `shouldBe` ["bbb"]
        it "first word complete, substring of the second word" $ do
          completions pt "something aa" `shouldBe` ["bbb"]
        it "first word complete, whole second word" $ do
          completions pt "something aaa" `shouldBe` ["bbb"]
        it "first and second word complete with space" $ do
          completions pt "something aaa " `shouldBe` ["ddd"]
        it "first and second word complete, substring of the third word" $ do
          completions pt "something aaa c" `shouldBe` ["ddd"]
        it "first and second word complete, whole third word" $ do
          completions pt "something aaa ccc" `shouldBe` ["ddd"]

    describe "alternative" $ do
      let p1 = prefix "something" (,) <*> token (symbol "aaa" >> pure 1) ["bbb"] <*> token (symbol "ccc" >> pure 2) ["ddd"]
          p2 = prefix "nothing" (3 :: Int,) <*> token (symbol "ggg" >> pure (4 :: Int)) ["xxx"]
          sum = p1 <|> p2
      it "prefixes" $ do
        prefixes sum `shouldBe` ["something", "nothing"]
      it "parser" $ do
        runParser (parser sum) "something aaa ccc" `shouldBe` Right (1, 2)
        runParser (parser sum) "nothing ggg" `shouldBe` Right (3, 4)
      describe "comptletion" $ do
        it "empty word" $ do
          completions sum "" `shouldBe` ["something", "nothing"]
        it "space" $ do
          completions sum " " `shouldBe` ["something", "nothing"]
        it "whole first word with space" $ do
          completions sum "something " `shouldBe` ["bbb"]
        it "whole second word with space" $ do
          completions sum "something aaa " `shouldBe` ["ddd"]
        it "first word, second case" $ do
          completions sum "nothing ggg " `shouldBe` ["xxx"]
