{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module TF where

import Data.Text
import GHC.Generics
import Text.Megaparsec
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Generics1

data CmdEnum
  = Plus
  | Minus
  | Result
  deriving stock (Show)

type family CmdResult (cmd :: CmdEnum) where
  CmdResult Plus = Int

-- this is broken unfortunately.
-- data Cmd = Cmd (k :: CmdEnum) (CmdResult k)

-- result :: Cmd -> CmdResult
-- result r = case

-- type Parser = Parsec Text Text

-- spaceConsumer :: Parser ()
-- spaceConsumer = Lexer.space Char.space1 Mega.empty Mega.empty

-- symbol :: Text -> Parser Text
-- symbol = Lexer.symbol spaceConsumer

-- cmdP :: Parser Cmd
-- cmdP = do
--   Mega.choice [plusP, minusP, resultP]
--   where
--     plusP = Plus <$> (symbol "plus" >> Lexer.decimal)
--     minusP = Minus <$> (symbol "minus" >> Lexer.decimal)
--     resultP = Result <$ symbol "result"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
