module Clio.Parsing.Parsed
  ( Parsed(..)
  , hasOption
  , getOption
  , getVariadicArgument
  , getArgument
  , getRequiredArgument
  ) where

import Prelude
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.List (List)

import Clio.Parsing.Argument (ArgsMap, ArgValue(..))
import Clio.Config.Option (Name)
import Clio.Parsing.Option (OptionsMap)

newtype Parsed = Parsed { options :: OptionsMap, arguments :: ArgsMap }

-- | Takes a parsed config and an option name.
-- | Returns true if the option is present, else returns false.
-- |
-- | Use this to check if a switch was turned on:
-- | ```purescript
-- | hasOption parsed (Short 'a')
-- | ```
hasOption :: Parsed -> Name -> Boolean
hasOption (Parsed { options: o }) n = M.member n o

-- | Takes a parsed config and an option name.
-- | If the option is present and has a value,
-- | returns the value, else returns Nothing.
-- |
-- | Example:
-- | ```purescript
-- | getOption parsed (Short 'a')
-- | ```
getOption :: Parsed -> Name -> Maybe String
getOption (Parsed { options: o }) n = join $ M.lookup n o

-- | Takes a parsed config and an argument name.
-- | If the argument is present and isn't a variadic argument,
-- | returns its value, else returns Nothing.
-- |
-- | Example:
-- | ```purescript
-- | getArgument parsed "arg"
-- | ```
-- |
-- | This returns a `Maybe` even when trying to get required
-- | argument values because Clio is unable to ensure that
-- | the input name maps to an existing required argument
-- | in its config. This might change in the future for a more
-- | sophisticated design but, in the meantime, you can use
-- | `getRequiredArgument` for required arguments.
getArgument :: Parsed -> String -> Maybe String
getArgument (Parsed { arguments: a }) n = do
  v <- M.lookup n a
  case v of
    StringValue v' -> pure v'
    _ -> Nothing

-- | Takes a parsed config and an argument name.
-- | If the argument is present and is a variadic argument,
-- | returns its value, else returns Nothing.
-- |
-- | Example:
-- | ```purescript
-- | getVariadicArgument parsed "args"
-- | ```
-- TODO: return just a list instead? (Nil instead of Nothing)
getVariadicArgument :: Parsed -> String -> Maybe (List String)
getVariadicArgument (Parsed { arguments: a }) n = do
  v <- M.lookup n a
  case v of
    VariadicValue v' -> pure v'
    _ -> Nothing

-- | Takes a parsed config and an argument name.
-- | If the argument is present and isn't a variadic argument,
-- | returns its value, else throws.
-- |
-- | Since this is a partial function, you should only use it
-- | for required arguments. This way, the call will be safe
-- | because Clio ensures required arguments are present
-- | in order to return a parsed config.
-- |
-- | Example:
-- | ```purescript
-- | unsafePartial $ getRequiredArgument parsed "arg"
-- | ```
getRequiredArgument :: Partial => Parsed -> String -> String
getRequiredArgument p = fromJust <<< getArgument p
