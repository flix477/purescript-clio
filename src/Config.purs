module Clio.Config
  ( Config
  , config
  , config'
  , description
  , options
  , options'
  , arguments
  ) where

import Prelude
import Data.Map (Map)
import Data.Map (empty, fromFoldable) as M
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Foldable (class Foldable, foldl)
import Control.Monad.Writer (execWriter)

import Clio.Config.Option (Option, Name, OptionEntry)
import Clio.Config.Arguments
  ( Arguments
  , ArgumentBuilder
  )
import Clio.Config.ArgumentStateMachine (StartState(..))

type Config =
  { title :: String
  , description :: Maybe String
  , options :: Map Name Option -- what about options that can be repeated with different values
  , arguments :: Arguments
  }

defaultConfig :: String -> Config
defaultConfig t =
  { title: t
  , description: Nothing
  , options: M.empty
  , arguments: Nil
  }

-- | Takes the app's name and a Config update function.
-- |
-- | Returns a Config which has the update function applied on it.
-- | ```purescript
-- | config "app"
-- |   $   (description "An app description")
-- |   <<< (options
-- |     [ switch' $ Short 'a'
-- |     , switch' $ Long "option"
-- |     , value' $ Short 'v'
-- |     ]
-- |   )
-- |   <<< (arguments
-- |     $   variadic' "files"
-- |     >=> required' "destination"
-- |   )
-- | ```
config :: String -> (Config -> Config) -> Config
config t f = f $ defaultConfig t

-- | Takes the app's name and a list of Config updates.
-- |
-- | Returns a Config which has the updates applied on it.
-- | ```purescript
-- | config' "app"
-- |   [ description "An app description"
-- |   , options
-- |     [ switch' $ Short 'a'
-- |     , switch' $ Long "option"
-- |     , value' $ Short 'v'
-- |     ]
-- |   , arguments
-- |     $   variadic' "files"
-- |     >=> required' "destination"
-- |   ]
config' :: forall f. Foldable f => String -> f (Config -> Config) -> Config
config' t fs = config t $ applyAll fs

-- | Sets the app's description that will be seen in the help dialog.
-- | ```purescript
-- | config "app" $ description "An app description"
-- | ```
description :: String -> Config -> Config
description d = _ { description = Just d }

-- | Sets the app's options from a list of option entries.
-- |
-- | Use this with the functions in `Clio.Config.Option`
-- | to create your options.
-- | ```purescript
-- | config "app" $ options
-- |   [ switch' $ Short 'a'
-- |   , switch' $ Long "option"
-- |   , value' $ Short 'v'
-- |   ]
-- | ```
options :: forall f. Foldable f => f OptionEntry -> Config -> Config
options = options' <<< M.fromFoldable

-- | Sets the app's options directly.
options' :: Map Name Option -> Config -> Config
options' o = _ { options = o }

-- | Sets the app's arguments using an argument builder.
-- |
-- | Use this with the functions in `Clio.Config.Arguments`
-- | to create your arguments list.
-- | ```purescript
-- | config "app" $
-- |   arguments (variadic' "files" >=> required' "destination")
-- | ```
-- |
-- | This argument builder prevents you at compile-time
-- | from making impossible parsing scenarios.
-- | An example would be trying to have an optional argument
-- | followed by a variadic argument. It would be impossible
-- | for a parser to determine whether the first input argument
-- | is the optional argument or the first item of the
-- | variadic argument. In code:
-- | ```purescript
-- | arguments (optional' "optionalFile" >=> variadic' "files")
-- | ```
-- | The above, instead of crashing the program at runtime,
-- | gives you a compile-time error with Clio.
arguments
  :: forall a. (StartState -> ArgumentBuilder a)
  -> Config
  -> Config
arguments f = arguments' $ execWriter (f StartState)

arguments' :: Arguments -> Config -> Config
arguments' args = _ { arguments = args }

applyAll :: forall f a. Foldable f => f (a -> a) -> a -> a
applyAll fs acc = foldl (#) acc fs
