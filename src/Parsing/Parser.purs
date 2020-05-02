module Clio.Parsing.Parser
  ( parse
  , ParseError(..)
  ) where

import Prelude
import Data.Either (Either(..))
import Data.List (List)
import Data.Map as M

import Clio.Config (Config)
import Clio.Config.Option (OptionEntry)
import Clio.Parsing.Option (parseOptions, OptionParseError)
import Clio.Parsing.Argument (ArgParseError)
import Clio.Parsing.Argument (parse) as A
import Clio.Parsing.Parsed (Parsed(..))

-- | Represents a parsing error.
data ParseError
  = OptionError OptionParseError
  | ArgumentError ArgParseError

mapErr :: forall e a c. (e -> c) -> Either e a -> Either c a
mapErr f (Left x) = Left $ f x
mapErr _ (Right x) = pure x

-- | Takes a config and a list of input arguments.
-- | Returns either a parsed config or a parsing error.
-- |
-- | On success, you can use the functions in `Clio.Parsing.Parsed`
-- | to get the parsed values for your options and arguments.
parse :: Config -> List String -> Either ParseError Parsed
parse c xs = do
  options <- mapErr (OptionError) $ parseOptions opts xs
  arguments <- mapErr (ArgumentError) $ A.parse c.arguments xs
  pure $ Parsed { options: options, arguments: arguments }
  where opts = M.toUnfoldableUnordered c.options :: List OptionEntry
