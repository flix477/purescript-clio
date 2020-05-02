module Clio.Parsing.Option
  ( parseOptions
  , OptionsMap
  , ParsedValue
  , OptionParseError(..)
  , ParsedKey(..)
  ) where

import Prelude

import Data.Tuple (Tuple(..), fst)
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Either (Either, note)
import Data.Foldable (find)
import Data.List (List(..), (:))
import Data.String as String
import Data.String.CodeUnits (toChar)
import Data.String.CodePoints (codePointFromChar, CodePoint)

import Clio.Config.Option (OptionEntry, Name(..))

data OptionParseError
  = MissingValue OptionEntry
  | InvalidOption ParsedKey

instance showOptionParseError :: Show OptionParseError where
  show (MissingValue o) = show o
  show (InvalidOption name) = show name

instance eqOptionParseError :: Eq OptionParseError where
  eq (MissingValue a) (MissingValue b) = eq a b
  eq (InvalidOption a) (InvalidOption b) = eq a b
  eq _ _ = false

type ParsedValue = Maybe String
type ParsedOption = Tuple Name ParsedValue
type OptionsMap = Map Name ParsedValue
type ParserResult = Either OptionParseError OptionsMap
data ParsedKey = ParsedShort Char | ParsedLong String

instance showParsedKey :: Show ParsedKey where
  show (ParsedShort c) = show c
  show (ParsedLong l) = show l

instance eqParsedKey :: Eq ParsedKey where
  eq (ParsedShort a) (ParsedShort b) = eq a b
  eq (ParsedLong a) (ParsedLong b) = eq a b
  eq _ _ = false

parseOptions
  :: List OptionEntry
  -> List String
  -> Either OptionParseError OptionsMap
parseOptions options inputs =
  parseOptions' options inputs M.empty

parseOptions'
  :: List OptionEntry
  -> List String
  -> OptionsMap
  -> ParserResult
parseOptions' _ Nil acc = pure acc
parseOptions' options (input:inputs) acc
  | Just k <- parseKey input = parseOption options inputs k acc
  | otherwise = parseOptions' options inputs acc

parseOption
  :: List OptionEntry
  -> List String
  -> ParsedKey
  -> OptionsMap
  -> ParserResult
parseOption options inputs k acc = do
  entry@(Tuple n { value: value }) <- findOption options k
  case value of
    Just _ -> do
      (Tuple head tail) <- note (MissingValue entry) $ next inputs
      continue tail n $ Just head
    _ -> continue inputs n Nothing
  where continue ins n v = parseOptions' options ins $ M.insert n v acc

findOption
  :: List OptionEntry
  -> ParsedKey
  -> Either OptionParseError OptionEntry
findOption options name = note (InvalidOption name)
  $ matchOpt name options

dash :: CodePoint
dash = codePointFromChar '-'

parseKey :: String -> Maybe ParsedKey
parseKey a =
  let
    dashCount = String.countPrefix ((==) dash) a
    trimmed = String.drop dashCount a
  in case dashCount of
    1 -> ParsedShort <$> toChar trimmed
    2 -> Just $ ParsedLong trimmed
    _ -> Nothing

matchOpt :: ParsedKey -> List OptionEntry -> Maybe OptionEntry
matchOpt k = find (\x -> keyEqualsName k $ fst x)

keyEqualsName :: ParsedKey -> Name -> Boolean
keyEqualsName (ParsedShort k) (Short n) = n == k
keyEqualsName (ParsedLong k) (Long n) = n == k
keyEqualsName (ParsedShort k) (Full n _) = n == k
keyEqualsName (ParsedLong k) (Full _ n) = n == k
keyEqualsName _ _ = false

next :: forall a. List a -> Maybe (Tuple a (List a))
next Nil = Nothing
next (x:xs) = pure $ Tuple x xs
