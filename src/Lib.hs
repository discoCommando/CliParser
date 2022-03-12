{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import CliParser
import Common2
import Data.Text
import Text.Megaparsec
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Cmd
  = Plus Int
  | Minus Int
  | Result
  deriving stock (Show)

cmdP :: CliParser Text Cmd
cmdP =
  (command "plus" Plus <*> argument Lexer.decimal "arg")
    <|> (command "minus" Minus <*> argument Lexer.decimal "arg")
    <|> command "result" Result

-- Mega.choice [plusP, minusP, resultP]
-- where
--   plusP = Plus <$> (symbol "plus" >> Lexer.decimal)
--   minusP = Minus <$> (symbol "minus" >> Lexer.decimal)
--   resultP = Result <$ symbol "result"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
