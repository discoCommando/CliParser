{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

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

cmdP :: Parser Text Cmd
cmdP = do
  Mega.choice [plusP, minusP, resultP]
  where
    plusP = Plus <$> (symbol "plus" >> Lexer.decimal)
    minusP = Minus <$> (symbol "minus" >> Lexer.decimal)
    resultP = Result <$ symbol "result"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
