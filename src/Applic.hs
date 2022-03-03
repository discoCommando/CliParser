module Applic where

import Common2 (Parser, runParser, spaceConsumer, symbol)
import Control.Lens ((%~), (&), (^.), view)
import Control.Monad.State ()
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import qualified Data.Map.Strict as Map
import Data.Text (Text, last, unwords, words)
import GHC.Base (Alternative ((<|>), empty), NonEmpty ((:|)))
import GHC.Generics (Generic)
import Prettyprinter
  ( Pretty (pretty),
    angles,
    encloseSep,
    hsep,
    lbrace,
    pipe,
    rbrace,
    vsep,
  )
import qualified Text.Megaparsec as Mega

newtype Prefix = Prefix (NonEmpty Text)
  deriving newtype (Eq, Ord)

instance Semigroup Prefix where
  (Prefix p1) <> (Prefix p2) = Prefix (p1 <> p2)

instance Pretty Prefix where
  pretty (Prefix prefixes) = case prefixes of
    x :| [] -> pretty x
    x :| xs -> encloseSep lbrace rbrace pipe $ pretty <$> (x : xs) -- {x | y | z}

newtype Description = Description (Maybe Text)

instance Semigroup Description where
  (Description p1) <> (Description p2) = Description (p1 <> p2)

instance Monoid Description where
  mempty = Description mempty

instance Pretty Description where
  pretty (Description md) = maybe mempty pretty md

newtype TokenName = TokenName Text

instance Pretty TokenName where
  pretty (TokenName tokenName) = angles $ pretty tokenName

data CommandData = CommandData
  { prefix :: Prefix,
    description :: Description,
    tokenNames :: [TokenName]
  }
  deriving stock (Generic)

instance Pretty CommandData where
  pretty cd =
    vsep
      [ hsep
          (pretty (cd ^. #prefix) : fmap pretty (cd ^. #tokenNames)),
        pretty $
          cd ^. #description
      ]

-- instance Semigroup CommandData where
--   cd1 <> cd2 = CommandData {
--     prefix' = view #prefix cd1 <> view #prefix cd2,
--     description =
--   }

data Command

data Token

type family DataForLabel a

type instance DataForLabel Command = CommandData

type instance DataForLabel Token = TokenName

-- | CliParser is a main structure that defines our
-- parsers' internals.
-- important: It relies heavily on the fact that commands
-- start from the prefix, and ends with tokens.
data CliParser label a = CliParser
  { commandsData :: Map.Map Prefix CommandData,
    -- | used for applicative instance
    currentData :: DataForLabel label,
    parser :: Parser a,
    completion :: Parser [Text]
  }
  deriving stock (Generic)

prefixCompletionParser :: Parser ()
prefixCompletionParser = spaceConsumer

command :: Text -> Maybe Text -> a -> CliParser Command a
command prefix' mdescription a =
  CliParser
    { commandsData = Map.singleton prefix'' currentCommandData,
      parser = a <$ symbol prefix',
      completion = prefixCompletionParser $> [prefix'],
      currentData = currentCommandData
    }
  where
    prefix'' = Prefix (prefix' :| [])
    currentCommandData =
      CommandData
        { prefix = prefix'',
          description = Description mdescription,
          tokenNames = []
        }

token :: Parser a -> Text -> [Text] -> CliParser Token a
token p tokenName cpl =
  CliParser
    { commandsData = mempty,
      parser = p,
      completion = pure cpl,
      currentData = TokenName tokenName
    }

-- | recover
-- A function that returns a parser that consumes an input if succeeded
-- and does not consume any input if failed (and returning an empty list)
recover :: Parser [Text] -> Parser [Text]
recover = fmap (fromRight []) . Mega.observing . Mega.try

completionsHelper :: Text -> Text
completionsHelper t
  | Prelude.null ws = t
  | Data.Char.isSpace $ Data.Text.last t = Data.Text.unwords ws
  | otherwise = Data.Text.unwords $ Prelude.init ws
  where
    ws = Data.Text.words t

completions :: CliParser Command a -> Text -> [Text]
completions cp =
  fromRight [] . runParser (completion cp) . completionsHelper

instance Functor (CliParser l) where
  fmap f cli = cli & #parser %~ fmap f

-- | This function mimicks the Applicative instance.
--  But to make it impossible to mess up the order (first command, then tokens)
--  CliParser has `label` type parameter. It makes it impossible to
--  write the Applicative instance for it.
(<*>) :: CliParser Command (a -> b) -> CliParser Token a -> CliParser Command b
cli1 <*> cli2 =
  CliParser
    { commandsData = Map.insert (newCurrentData ^. #prefix) newCurrentData $ cli1 ^. #commandsData,
      parser = view #parser cli1 Prelude.<*> view #parser cli2,
      completion = do
        -- take the completion from parser cli1, assumming that it will
        -- succeed with eof at the end
        cli1Result <- recover (view #completion cli1 <* Mega.eof)
        -- take the
        cli2Result <- recover (view #parser cli1 *> view #completion cli2)
        pure (cli1Result ++ cli2Result),
      currentData = newCurrentData
    }
  where
    newCurrentData = view #currentData cli1 & #tokenNames %~ (++ [view #currentData cli2])

-- instance Applicative CliParser where
--   pure a =
--     CliParser
--       { commandsData = mempty,
--         parser = pure a,
--         completion = mempty,
--         currentCommandData =
--           CommandData
--             { prefix' = mempty,
--               description = Description mempty,
--               tokenNames = mempty
--             }
--       }

--   cli1 <*> cli2 =
--     CliParser
--       { commandsData = Map.insert (currentCommandData ^. #prefix') currentCommandData $ cli1 ^. #commandsData,
--         parser = view #parser cli1 <*> view #parser cli2,
--         completion = do
--           -- take the completion from parser cli1, assumming that it will
--           -- succeed with eof at the end
--           cli1Result <- recover (view #completion cli1 <* Mega.eof)
--           -- take the
--           cli2Result <- recover (view #parser cli1 *> view #completion cli2)
--           pure (cli1Result ++ cli2Result),
--         currentCommandData = currentCommandData
--       }
--     where
--       currentCommandData = view #currentCommandData cli1 & #tokenNames %~ (++ view (#currentCommandData . #tokenNames) cli2)

-- instance Alternative CliParser where
--   (CliParser pr1 p1 c1) <|> (CliParser pr2 p2 c2) = CliParser (pr1 <> pr2) (p1 <|> p2) ((++) <$> Mega.lookAhead (recover c1) <*> Mega.lookAhead (recover c2))
--   empty = CliParser mempty empty empty

-- how do i want to do this

-- x = Prefix Plus "plus" <*> Token (..) []
