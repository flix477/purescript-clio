module Clio.Config.Option
  ( Name(..)
  , OptionEntry
  , Option
  , OptionValue
  , description
  , switch
  , value
  , switch'
  , value'
  , placeholder
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type OptionEntry = Tuple Name Option

-- | Represents the name of an option.
-- |
-- | `Short 'o'` will match `-o`
-- |
-- | `Long "option"` will match `--option`
-- |
-- | `Full 'o' "option"` will match both `-o` and `--option`
-- |
-- | Note that you should probably store your option
-- | and argument names as constants instead of copying them,
-- | allowing you to more easily change them in the future
-- | if the need arises.
data Name = Short Char | Long String | Full Char String

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name
instance showName :: Show Name where
  show (Short c) = show c
  show (Long l) = l
  show (Full c l) = (show c) <> ", " <> l

type OptionValue = { placeholder :: String }
type Option =
  { description :: Maybe String
  , value :: Maybe OptionValue
  }

defaultSwitch :: Option
defaultSwitch = { description: Nothing, value: Nothing }

defaultValue :: Option
defaultValue =
  { description: Nothing
  , value: Just { placeholder: "VALUE" }
  }

-- | Sets the option's description that will
-- | be seen in the help dialog.
description :: String -> Option -> Option
description d = _ { description = Just d }

-- | Sets the placeholder displayed in the help dialog
-- | for the option's value. This has no effect for switches.
-- |
-- | ```purescript
-- | value (Short 'c') $ placeholder "CONFIG"
-- | ```
-- |
-- | In the help dialog, this will show as: `-c CONFIG`
-- TODO: that kind of sucks tho, shouldn't be able to compile on switches
placeholder :: String -> Option -> Option
placeholder p o = o { value = _ { placeholder = p } <$> o.value }

-- | Takes a name and fields.
-- | Creates an option that takes no values; a simple flag.
-- |
-- | Example:
-- | ```purescript
-- | switch (Short 'c') $ description "An option"
-- | ```
-- | This will match a simple `-c`
switch :: Name -> (Option -> Option) -> OptionEntry
switch n f = Tuple n $ f defaultSwitch

-- | Takes only a name, no fields.
-- | Creates an option that takes no values; a simple flag.
-- |
-- | This is equivalent to calling `switch` with `identity`
-- | as a second parameter.
-- |
-- | Example:
-- | ```purescript
-- | switch' $ Short 'c'
-- | ```
-- | This will match a simple `-c`
switch' :: Name -> OptionEntry
switch' n = Tuple n defaultSwitch

-- | Takes a name and fields.
-- | Creates an option that takes a value.
-- |
-- | Example:
-- | ```purescript
-- | value (Short 'c') $ description "An option that takes a value"
-- | ```
-- | This will match on `-c VALUE`
-- TODO: should have a Foldable f (Option -> Option) variant?
value :: Name -> (Option -> Option) -> OptionEntry
value n f = Tuple n $ f defaultValue

-- | Takes only a name, no fields.
-- | Creates an option that takes a value.
-- |
-- | This is equivalent to calling `value` with `identity`
-- | as a second parameter.
-- |
-- | Example:
-- | ```purescript
-- | value' $ Short 'c'
-- | ```
-- | This will match on `-c VALUE`
value' :: Name -> OptionEntry
value' n = Tuple n defaultValue
