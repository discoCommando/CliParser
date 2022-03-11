module CliParserSpec where

import CliParser
import Common2 (runParser, symbol)
import Data.Functor (($>))
import Data.List.NonEmpty (toList)
import Data.Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Text.Megaparsec as Mega
import Prelude hiding ((<*>))

spec :: SpecWith ()
spec = do
  describe "cliParser" $ do
    describe "prefix" $ do
      let p = "test"
          t = commandWithMods p () (withDescription "description" <> withAlias ["alternative", ":t"])

      it "parser" $ do
        runParser (parser t) p `shouldBe` Right ()
      it "prefix" $ do
        _prefixes t `shouldBe` [["test", "alternative", ":t"]]
      it "prefix" $ do
        _descriptions t `shouldBe` [(["test", "alternative", ":t"], Just "description")]
      describe "completion" $ do
        it "empty word" $ do
          completions t "" `shouldBe` ["test", "alternative", ":t"]
        it "non empty word" $ do
          completions t "te" `shouldBe` ["test"]
          completions t ":" `shouldBe` [":t"]
          completions t "a" `shouldBe` ["alternative"]

    describe "argument" $ do
      let p = "dupa"
          t = argumentWithMods (symbol p >> pure ()) "" $ withCompletions ["aaa", "bbb"]

      it "parser" $ do
        runParser (parser t) p `shouldBe` Right ()
      it "completion" $ do
        runParser (completion t) "" `shouldBe` Right ["aaa", "bbb"]

    describe "completionsHelper" $ do
      it "empty" $ do
        completionsHelper "" `shouldBe` ("", "")
      it "first word" $
        completionsHelper "a" `shouldBe` ("", "a")
      it "first word with space" $
        completionsHelper "a " `shouldBe` ("a", "")
      it "two words" $
        completionsHelper "a a" `shouldBe` ("a", "a")
      it "three words" $
        completionsHelper "abc efg hjk" `shouldBe` ("abc efg", "hjk")

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
      let pt = command "something" (,) <*> argumentWithMods (symbol "aaa" >> pure ()) "" (withCompletions ["aaa", "aaaa"]) <*> argumentWithMods (symbol "ccc" >> pure ()) "" (withCompletions ["ccc", "cc"])
      it "parser" $ do
        runParser (parser pt) "something aaa ccc" `shouldBe` Right ((), ())
      it "prefix" $ do
        _prefixes pt `shouldBe` [["something"]]
      describe "completion" $ do
        it "empty word" $ do
          completions pt "" `shouldBe` ["something"]
        it "substring of the first word" $ do
          completions pt "somethin" `shouldBe` ["something"]
        it "whole first word" $ do
          completions pt "something" `shouldBe` ["something"]
        it "first word complete with space" $ do
          completions pt "something " `shouldBe` ["aaa", "aaaa"]
        it "first word complete, substring of the second word" $ do
          completions pt "something aa" `shouldBe` ["aaa", "aaaa"]
        it "first word complete, whole second word" $ do
          completions pt "something aaa" `shouldBe` ["aaa", "aaaa"]
        it "first and second word complete with space" $ do
          completions pt "something aaa " `shouldBe` ["ccc", "cc"]
        it "first and second word complete, substring of the third word" $ do
          completions pt "something aaa c" `shouldBe` ["ccc", "cc"]
        it "first and second word complete, whole third word" $ do
          completions pt "something aaa ccc" `shouldBe` ["ccc"]

    describe "alternative" $ do
      let --p1 :: CliParser Command (Int, Int)
          p1 = command "something" (,) <*> argumentWithMods (symbol "aaa" >> pure (1 :: Int)) "" (withCompletions ["bbb"]) <*> argumentWithMods (symbol "ccc" >> pure (2 :: Int)) "" (withCompletions ["ddd"])
          -- p2 :: CliParser Command (Int, Int)
          p2 = command "nothing" ((,) (3 :: Int)) <*> argumentWithMods (symbol "ggg" >> pure (4 :: Int)) "" (withCompletions ["xxx"])
          -- sum' :: CliParser Commands (Int, Int)
          sum' = p1 <|> p2
      it "prefixes" $ do
        _prefixes sum' `shouldBe` [["nothing"], ["something"]]
      it "parser" $ do
        runParser (parser sum') "something aaa ccc" `shouldBe` Right (1 :: Int, 2 :: Int)
        runParser (parser sum') "nothing ggg" `shouldBe` Right (3 :: Int, 4 :: Int)
      describe "completion" $ do
        it "empty word" $ do
          completions sum' "" `shouldBe` ["something", "nothing"]
        it "space" $ do
          completions sum' " " `shouldBe` ["something", "nothing"]
        it "whole first word with space" $ do
          completions sum' "something " `shouldBe` ["bbb"]
        it "whole second word with space" $ do
          completions sum' "something aaa " `shouldBe` ["ddd"]
        it "first word, second case" $ do
          completions sum' "nothing ggg " `shouldBe` ["xxx"]
