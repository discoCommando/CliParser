module Applic where

import Common2 (Parser, runParser, spaceConsumer, symbol)
import Control.Lens ((%~), (&), (.~), (^.), _1, _2, view)
import Control.Monad.State ()
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import Data.List.NonEmpty (toList)
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
import Prelude hiding (mod)

newtype Prefix = Prefix {unprefix :: NonEmpty Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)

concatPrefix :: [Text] -> Prefix -> Prefix
concatPrefix ts (Prefix (t :| rest)) = Prefix (t :| rest ++ ts)

instance Semigroup Prefix where
  (Prefix p1) <> (Prefix p2) = Prefix (p1 <> p2)

instance Pretty Prefix where
  pretty (Prefix prefixes) = case prefixes of
    x :| [] -> pretty x
    x :| xs -> encloseSep lbrace rbrace pipe $ pretty <$> (x : xs) -- {x | y | z}

newtype Description = Description {undescription :: Text}
  deriving stock (Generic)

instance Semigroup Description where
  (Description p1) <> (Description p2) = Description (p1 <> p2)

instance Monoid Description where
  mempty = Description mempty

instance Pretty Description where
  pretty (Description md) = pretty md

newtype TokenName = TokenName Text

instance Pretty TokenName where
  pretty (TokenName tokenName) = angles $ pretty tokenName

data CommandData = CommandData
  { prefix :: Prefix,
    description :: Maybe Description,
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

data Command -- A command

data Token -- One token

data Commands -- More than one command

-- This is a type family that denotes the data
-- that needs to be carried for each label type
type family DataForLabel l

type instance
  DataForLabel Command =
    CommandData -- command data for the current command

type instance DataForLabel Token = TokenName

type instance DataForLabel Commands = Map.Map Prefix CommandData

type family Mod' a

type instance
  Mod' Command =
    ( [Text], -- aliases
      Maybe Description -- description
    )

type instance Mod' Token = [Text] -- completions

-- | CliParser is a main structure that defines our
-- parsers' internals.
-- important: It relies heavily on the fact that commands
-- start from the prefix, and ends with tokens.
data CliParser label a = CliParser
  { currentData :: DataForLabel label,
    parser :: Parser a,
    completion :: Parser [Text]
  }
  deriving stock (Generic)

prefixCompletionParser :: Parser ()
prefixCompletionParser = spaceConsumer

newtype Mod a = Mod {unmod :: Mod' a}
  deriving stock (Generic)

deriving newtype instance Semigroup (Mod' a) => Semigroup (Mod a)

deriving newtype instance Monoid (Mod' a) => Monoid (Mod a)

command :: Text -> a -> CliParser Command a
command prefix' a =
  commandWithMods prefix' a mempty

commandWithMods :: Text -> a -> Mod Command -> CliParser Command a
commandWithMods prefix' a mod =
  CliParser
    { parser = a <$ symbol prefix',
      completion = prefixCompletionParser $> [prefix'],
      currentData = currentCommandData
    }
  where
    prefix'' = Prefix (prefix' :| (unmod mod ^. _1))
    currentCommandData =
      CommandData
        { prefix = prefix'',
          description = unmod mod ^. _2,
          tokenNames = []
        }

token :: Parser a -> Text -> CliParser Token a
token p tokenName =
  tokenWithMods p tokenName mempty

tokenWithMods :: Parser a -> Text -> Mod Token -> CliParser Token a
tokenWithMods p tokenName mod =
  CliParser
    { parser = p,
      completion = pure (unmod mod),
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
    { parser = view #parser cli1 Prelude.<*> view #parser cli2,
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
    newCurrentData :: CommandData
    newCurrentData = view #currentData cli1 & #tokenNames %~ (++ [view #currentData cli2])

class ToCliCommands l where
  upgradeToCliCommands :: CliParser l a -> CliParser Commands a

instance ToCliCommands Command where
  upgradeToCliCommands cli =
    cli {currentData = Map.singleton (currentData cli ^. #prefix) (cli ^. #currentData)}

instance ToCliCommands Commands where
  upgradeToCliCommands = id

class CliAlternative a b r where
  (<|>) :: a -> b -> r

instance (ToCliCommands l1, ToCliCommands l2) => CliAlternative (CliParser l1 a) (CliParser l2 a) (CliParser Commands a) where
  cli1 <|> cli2 =
    CliParser
      { currentData = view #currentData cli1' <> view #currentData cli2',
        parser = view #parser cli1' GHC.Base.<|> view #parser cli2',
        completion = do
          r1 <- Mega.lookAhead . recover $ view #completion cli1'
          r2 <- Mega.lookAhead . recover $ view #completion cli2'
          pure (r1 ++ r2)
      }
    where
      cli1' = upgradeToCliCommands cli1
      cli2' = upgradeToCliCommands cli2
