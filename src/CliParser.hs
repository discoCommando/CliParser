module CliParser where

import Common2 (Parser, runParser, spaceConsumer, symbol)
import Control.Lens ((%~), (&), (.~), (^.), (^?), _1, _2, to, view)
import Control.Monad.State ()
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text, last, unwords, words)
import qualified Data.Text as Text
import qualified Debug.Trace as Trace
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

newtype ArgumentName = ArgumentName Text

instance Pretty ArgumentName where
  pretty (ArgumentName argumentName) = angles $ pretty argumentName

data CommandData = CommandData
  { prefix :: Prefix,
    description :: Maybe Description,
    argumentNames :: [ArgumentName]
  }
  deriving stock (Generic)

instance Pretty CommandData where
  pretty cd =
    vsep
      [ hsep
          (pretty (cd ^. #prefix) : fmap pretty (cd ^. #argumentNames)),
        pretty $
          cd ^. #description
      ]

data Command -- A command

data Argument -- One argument

data Commands -- More than one command

type family Mod' a

type instance
  Mod' Command =
    ( [Text], -- aliases
      Maybe Description -- description
    )

type instance Mod' Argument = [Text] -- completions

data Token = Arg ArgumentName | Pref Prefix
  deriving stock (Generic)

data TextualRep = TextualRep
  { tokens :: [Token],
    description :: Maybe Description
  }
  deriving stock (Generic)

instance Semigroup TextualRep where
  a <> b =
    TextualRep
      { tokens = a ^. #tokens <> b ^. #tokens,
        description = a ^. #description -- we take the description from the first text rep only
      }

-- | CliParser is a main structure that defines our
-- parsers' internals.
-- important: It relies heavily on the fact that commands
-- start from the prefix, and ends with arguments.
data CliParser err a = CliParser
  { textualRep :: [TextualRep],
    parser :: Parser err a,
    completion :: Parser err [Text]
  }
  deriving stock (Generic)

prefixCompletionParser :: Ord err => Parser err ()
prefixCompletionParser = spaceConsumer

newtype Mod a = Mod {unmod :: Mod' a}
  deriving stock (Generic)

deriving newtype instance Semigroup (Mod' a) => Semigroup (Mod a)

deriving newtype instance Monoid (Mod' a) => Monoid (Mod a)

command :: Ord err => Text -> a -> CliParser err a
command prefix' a =
  commandWithMods prefix' a mempty

commandWithMods :: Ord err => Text -> a -> Mod Command -> CliParser err a
commandWithMods prefix' a mod =
  CliParser
    { parser = a <$ symbol prefix',
      completion = prefixCompletionParser $> (prefix' : unmod mod ^. _1),
      textualRep =
        [ TextualRep
            { tokens = [Pref prefix''],
              description = unmod mod ^. _2
            }
        ]
    }
  where
    prefix'' = Prefix (prefix' :| (unmod mod ^. _1))

withAlias :: [Text] -> Mod Command
withAlias t = Mod (t, Nothing)

withDescription :: Text -> Mod Command
withDescription t = Mod ([], Just $ Description t)

argument :: Parser err a -> Text -> CliParser err a
argument p argumentName =
  argumentWithMods p argumentName mempty

argumentWithMods :: Parser err a -> Text -> Mod Argument -> CliParser err a
argumentWithMods p argumentName mod =
  CliParser
    { parser = p,
      completion = pure (unmod mod),
      textualRep =
        [ TextualRep
            { tokens = [Arg $ ArgumentName argumentName],
              description = Nothing -- for now, perhaps add it in the future?
            }
        ]
    }

withCompletions :: [Text] -> Mod Argument
withCompletions = Mod

-- | recover
-- A function that returns a parser that consumes an input if succeeded
-- and does not consume any input if failed (and returning an empty list)
recover :: Ord err => Parser err [Text] -> Parser err [Text]
recover = fmap (fromRight []) . Mega.observing . Mega.try

completionsHelper :: Text -> (Text, Text)
completionsHelper t
  | Prelude.null ws = (t, "")
  | Data.Char.isSpace $ Data.Text.last t = (Data.Text.unwords ws, "")
  | otherwise = (Data.Text.unwords $ Prelude.init ws, Prelude.last ws)
  where
    ws = Data.Text.words t

completions :: CliParser err a -> Text -> [Text]
completions cp input =
  filter (Text.isPrefixOf incompleteInput) . Trace.traceShow incompleteInput . Trace.traceShowId . fromRight [] . Mega.runParser (completion cp) "completions" $ completedInput
  where
    (completedInput, incompleteInput) = completionsHelper input

instance Functor (CliParser l) where
  fmap f cli = cli & #parser %~ fmap f

-- | This function mimicks the Applicative instance.
--  But to make it impossible to mess up the order (first command, then arguments)
--  CliParser has `label` type parameter. It makes it impossible to
--  write the Applicative instance for it.
instance (Ord err) => Applicative (CliParser err) where
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
        textualRep =
          case cli1 ^. #textualRep of
            [x] ->
              case cli2 ^. #textualRep of
                [y] ->
                  [x <> y] -- happy path
                _ ->
                  [x] -- should not really happen
            _ -> cli1 ^. #textualRep
      }
  pure a =
    CliParser
      { parser = pure a,
        completion = pure [],
        textualRep = []
      }

instance (Ord err) => Alternative (CliParser err) where
  cli1 <|> cli2 =
    CliParser
      { textualRep = view #textualRep cli1 <> view #textualRep cli2,
        parser = view #parser cli1 GHC.Base.<|> view #parser cli2,
        completion = do
          r1 <- Mega.lookAhead . recover $ view #completion cli1
          r2 <- Mega.lookAhead . recover $ view #completion cli2
          pure (r1 ++ r2)
      }
  empty =
    -- better not use it I guess
    CliParser
      { textualRep = empty,
        parser = empty,
        completion = empty
      }

-- helper for the tests
_prefixes :: CliParser err a -> [[Text]]
_prefixes cli =
  let textualReps = cli ^. #textualRep
      safeHead = \case
        x : _rest -> Just x
        _ -> Nothing
   in mapMaybe (\x -> x ^? #tokens . to safeHead . #_Just . #_Pref . to (toList . unprefix)) textualReps

_descriptions :: CliParser err a -> [([Text], Maybe Text)]
_descriptions cli =
  let textualReps = cli ^. #textualRep
      safeHead = \case
        x : _rest -> Just x
        _ -> Nothing
   in mapMaybe (\x -> fmap (,x ^? #description . #_Just . #undescription) (x ^? #tokens . to safeHead . #_Just . #_Pref . to (toList . unprefix))) textualReps
