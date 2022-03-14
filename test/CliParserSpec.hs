module CliParserSpec where

import CliParser
import Common2 (runParser, symbol)
import Control.Applicative ((<|>))
import Data.Either (isLeft)
import Data.Functor (($>))
import Data.List.NonEmpty (toList)
import Data.Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Text.Megaparsec as Mega

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
      it "descriptions" $ do
        _descriptions t `shouldBe` [[(["test", "alternative", ":t"], Just "description")]]
      describe "completion" $ do
        it "empty word" $ do
          completions t "" `shouldBe` ["test", "alternative", ":t"]
        it "non empty word" $ do
          completions t "te" `shouldBe` ["test"]
          completions t ":" `shouldBe` [":t"]
          completions t "a" `shouldBe` ["alternative"]

    describe "argument" $ do
      let p = "dupa"
          t = argumentWithMods (symbol p >> pure ()) "name" $ withCompletions ["aaa", "bbb"] <> withDescription "dupa dupa"

      it "parser" $ do
        runParser (parser t) p `shouldBe` Right ()
      describe "completion" $ do
        it "empty" $
          completions t "" `shouldBe` ["aaa", "bbb"]
        it "first" $
          completions t "a" `shouldBe` ["aaa"]
        it "second" $
          completions t "b" `shouldBe` ["bbb"]
        it "none" $
          completions t "x" `shouldBe` []
      it "descriptions" $ do
        _descriptions t `shouldBe` [[(["name"], Just "dupa dupa")]]

    describe "optionalArgument" $ do
      let p = "dupa"
          t = optionalArgumentWithMods (symbol p >> pure ()) "name" $ withCompletions [p]

      it "parser" $ do
        runParser (parser t) p `shouldBe` Right (Just ())
        runParser (parser t) "xd" `shouldBe` Right Nothing
      it "completion" $ do
        completions t "" `shouldBe` [p]
        completions t "du" `shouldBe` [p]
        completions t "x" `shouldBe` []
      it "descriptions" $ do
        _descriptions t `shouldBe` [[(["name?"], Nothing)]]

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
        runParser (recover (symbol "aaa" $> ["bbb" :: Text])) "aaa" `shouldBe` Right ["bbb"]
      it "not happy path" $ do
        runParser (recover (symbol "aaa" $> ["bbb" :: Text])) "ccc" `shouldBe` Right []
      it "happy path does consumes an input" $ do
        runParser (recover (symbol "aaa" $> ["bbb" :: Text]) *> Mega.takeRest) "aaa ccc" `shouldBe` Right "ccc" -- not sure if that's correct though
      it "not happy path does not consume any input" $ do
        runParser (recover (symbol "aaa" $> ["bbb" :: Text]) *> Mega.takeRest) "xxx ccc" `shouldBe` Right "xxx ccc"

    describe "applicative" $ do
      let pt =
            commandWithMods "something" (,) (withDescription "sss" <> withAlias ["smth"])
              <*> optionalArgumentWithMods (symbol "aaa" >> pure ()) "aa" (withCompletions ["aaa", "aaaa"] <> withDescription "AAA")
              <*> argumentWithMods (symbol "ccc" >> pure ()) "cc" (withCompletions ["ccc", "cc"] <> withDescription "CCC")
      describe "parser" $ do
        it "just" $
          runParser (parser pt) "something aaa ccc" `shouldBe` Right (Just (), ())
        it "nothing" $
          runParser (parser pt) "something ccc" `shouldBe` Right (Nothing, ())
        it "nothing failed" $
          runParser (parser pt) "something xx ccc" `shouldSatisfy` isLeft
      it "prefix" $ do
        _prefixes pt `shouldBe` [["something", "smth"]]
      describe "completion" $ do
        it "empty word" $ do
          completions pt "" `shouldBe` ["something", "smth"]
        it "substring of the first word" $ do
          completions pt "somethin" `shouldBe` ["something"]
        it "whole first word" $ do
          completions pt "something" `shouldBe` ["something"]
        it "first word complete with space" $ do
          -- actually this is not correct,
          -- because there should be completions from the third
          -- token as well, but that's for another time
          -- I think it would require building
          -- parser with a special AST
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
        xit "first word complete, substring of the third word" $ do
          completions pt "something c" `shouldBe` ["ccc", "cc"]
        xit "first  complete, whole third word" $ do
          completions pt "something ccc" `shouldBe` ["ccc"]
      it "descriptions" $ do
        _descriptions pt `shouldBe` [[(["something", "smth"], Just "sss"), (["aa?"], Just "AAA"), (["cc"], Just "CCC")]]

    describe "alternative" $ do
      let --p1 :: CliParser Command (Int, Int)
          p1 = command "something" (,) <*> argumentWithMods (symbol "aaa" >> pure (1 :: Int)) "f" (withCompletions ["bbb"]) <*> argumentWithMods (symbol "ccc" >> pure (2 :: Int)) "arg2" (withCompletions ["ddd"] <> withDescription "argument numba 2")
          -- p2 :: CliParser Command (Int, Int)
          p2 = commandWithMods "nothing" (3 :: Int,) (withDescription "nothing there as well") <*> argumentWithMods (symbol "ggg" >> pure (4 :: Int)) "arg1" (withCompletions ["xxx"])
          -- sum' :: CliParser Commands (Int, Int)
          sum' = p1 <|> p2
      it "prefixes" $ do
        _prefixes sum' `shouldBe` [["something"], ["nothing"]]
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
      it "descriptions" $ do
        _descriptions sum' `shouldBe` [[(["something"], Nothing), (["f"], Nothing), (["arg2"], Just "argument numba 2")], [(["nothing"], Just "nothing there as well"), (["arg1"], Nothing)]]
