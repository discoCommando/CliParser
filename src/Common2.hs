module Common2 where

import Data.Text
import Text.Megaparsec
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Text Text

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space Char.space1 Mega.empty Mega.empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

runParser :: Parser a -> Text -> Either (Mega.ParseErrorBundle Text Text) a
runParser p = Mega.runParser p "p"
