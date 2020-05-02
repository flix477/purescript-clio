module Clio.Config.Argument
  ( Argument
  , description
  , ArgumentType(..)
  , ArgumentEntry
  , ArgumentFields
  , buildEntry
  , fields
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- TODO: this whole thing would need to be remade
--       for required variadics (1+)
data ArgumentType
  = Required
  | Variadic
  | Optional

instance showArgumentType :: Show ArgumentType where
  show Required = "Required"
  show Variadic = "Variadic"
  show Optional = "Optional"

instance eqArgumentType :: Eq ArgumentType where
  eq Required Required = true
  eq Variadic Variadic = true
  eq Optional Optional = true
  eq _ _ = false

type ArgumentFields = { description :: Maybe String }

type Argument =
  { description :: Maybe String
  , argumentType :: ArgumentType
  }

type ArgumentEntry = Tuple String Argument

defaultFields :: ArgumentFields
defaultFields = { description: Nothing }

-- | Sets the arguments's description that will
-- | be seen in the help dialog.
description :: String -> ArgumentFields -> ArgumentFields
description d = _ { description = Just d }

fields :: (ArgumentFields -> ArgumentFields) -> ArgumentFields
fields f = f defaultFields

applyFields :: ArgumentType -> ArgumentFields -> Argument
applyFields t x =
  { argumentType: t
  , description: x.description
  }

buildEntry
  :: ArgumentType
  -> String
  -> (ArgumentFields -> ArgumentFields)
  -> ArgumentEntry
buildEntry t n f = Tuple n (applyFields t $ fields f)
