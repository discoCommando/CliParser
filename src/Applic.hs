{-# LANGUAGE DerivingStrategies #-}

module Applic where

import Common2
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Text hiding (empty)
import GHC.Base (Alternative ((<|>), empty))
import qualified Text.Megaparsec as Mega

data CliParser a = CliParser
  {prefixes :: [Text], parser :: Parser a, completion :: Parser [Text]}

prefixCompletionParser :: Parser ()
prefixCompletionParser = spaceConsumer

prefix :: Text -> a -> CliParser a
prefix prefix a = CliParser [prefix] (a <$ symbol prefix) (prefixCompletionParser $> [prefix])

token :: Parser a -> [Text] -> CliParser a
token p completions = CliParser [] p $ pure completions

instance Functor CliParser where
  fmap f (CliParser pr p c) = CliParser pr (f <$> p) c

recover :: Parser [Text] -> Parser [Text]
recover = fmap (fromRight []) . Mega.observing . Mega.try

completionsHelper :: Text -> Text
completionsHelper t
  | Prelude.null ws = t
  | Data.Char.isSpace $ Data.Text.last t = Data.Text.unwords ws
  | otherwise = Data.Text.unwords $ Prelude.init ws
  where
    ws = Data.Text.words t

completions :: CliParser a -> Text -> [Text]
completions cp =
  fromRight [] . runParser (completion cp) . completionsHelper

instance Applicative CliParser where
  pure a = CliParser [] (pure a) (pure [])
  (CliParser pr1 p1 c1) <*> (CliParser pr2 p2 c2) =
    CliParser
      (pr1 ++ pr2)
      (p1 <*> p2)
      ((++) <$> recover (c1 <* Mega.eof) <*> recover (p1 *> c2))

instance Alternative CliParser where
  (CliParser pr1 p1 c1) <|> (CliParser pr2 p2 c2) = CliParser (pr1 ++ pr2) (p1 <|> p2) ((++) <$> Mega.lookAhead (recover c1) <*> Mega.lookAhead (recover c2))
  empty = CliParser [] empty empty

-- how do i want to do this

-- x = Prefix Plus "plus" <*> Token (..) []
