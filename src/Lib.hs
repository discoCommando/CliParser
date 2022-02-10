{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib
  ( someFunc,
  )
where

import Data.Data
import Data.Text
import Data.Typeable
import Text.Megaparsec
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Cmd
  = Plus Int
  | Minus Int
  | Result
  deriving stock (Show)
  deriving (Data, Typeable)

type Parser = Parsec Text Text

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space Char.space1 Mega.empty Mega.empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

cmdP :: Parser Cmd
cmdP = do
  Mega.choice [plusP, minusP, resultP]
  where
    plusP = Plus <$> (symbol "plus" >> Lexer.decimal)
    minusP = Minus <$> (symbol "minus" >> Lexer.decimal)
    resultP = Result <$ symbol "result"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
