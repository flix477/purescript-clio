module Clio.Config.Arguments
  ( required
  , optional
  , variadic
  , required'
  , optional'
  , variadic'
  , ArgumentBuilder
  , Arguments
  ) where

import Prelude
import Control.Monad.Writer (Writer, tell)
import Data.List (List, singleton)

import Clio.Config.Argument
  ( ArgumentEntry
  , buildEntry
  , ArgumentFields
  , ArgumentType(..)
  )
import Clio.Config.ArgumentStateMachine
  ( class StateMachine
  , InternalOptional(..)
  , InternalRequired(..)
  , InternalVariadic(..)
  , feed
  )

type Arguments = List ArgumentEntry
type ArgumentBuilder a = Writer Arguments a

-- | Creates a required argument with the specified name and fields.
-- |
-- | When parsing, Clio will ensure that required arguments are present,
-- | or else an error result will be returned.
-- |
-- | Example:
-- | ```purescript
-- | required "arg" $ description "An argument"
-- | ```
-- |
-- | Note that you should probably store your option
-- | and argument names as constants instead of copying them,
-- | allowing you to more easily change them in the future
-- | if the need arises.
required
  :: forall a b. StateMachine a InternalRequired b
  => String
  -> (ArgumentFields -> ArgumentFields)
  -> a
  -> ArgumentBuilder b
required = nextState InternalRequired Required

-- | Creates a required argument with the specified name and no fields.
-- |
-- | This is equivalent to calling `required` with `identity`
-- | as a second parameter.
-- |
-- | Example:
-- | ```purescript
-- | required' "arg"
-- | ```
required'
  :: forall a b. StateMachine a InternalRequired b
  => String
  -> a
  -> ArgumentBuilder b
required' n = required n identity

-- | Creates a variadic argument with the specified name and fields.
-- |
-- | Variadic arguments are arguments that can have zero to many values.
-- | For example, `vim` uses this to make it possible to edit
-- | multiple files in a sequence.
-- |
-- | Example:
-- | ```purescript
-- | variadic "files" $ description "Some files"
-- | ```
-- |
-- | Note that you should probably store your option
-- | and argument names as constants instead of copying them,
-- | allowing you to more easily change them in the future
-- | if the need arises.
variadic
  :: forall a b. StateMachine a InternalVariadic b
  => String
  -> (ArgumentFields -> ArgumentFields)
  -> a
  -> ArgumentBuilder b
variadic = nextState InternalVariadic Variadic

-- | Creates a variadic argument with the specified name and no fields.
-- |
-- | This is equivalent to calling `variadic` with `identity`
-- | as a second parameter.
-- |
-- | Example:
-- | ```purescript
-- | variadic' "files"
-- | ```
variadic'
  :: forall a b. StateMachine a InternalVariadic b
  => String
  -> a
  -> ArgumentBuilder b
variadic' n = variadic n identity

-- | Creates an optional argument with the specified name and fields.
-- |
-- | Example:
-- | ```purescript
-- | optional "arg" $ description "Some optional argument"
-- | ```
-- |
-- | Note that you should probably store your option
-- | and argument names as constants instead of copying them,
-- | allowing you to more easily change them in the future
-- | if the need arises.
optional
  :: forall a b. StateMachine a InternalOptional b
  => String
  -> (ArgumentFields -> ArgumentFields)
  -> a
  -> ArgumentBuilder b
optional = nextState InternalOptional Optional

-- | Creates an optional argument with the specified name and no fields.
-- |
-- | This is equivalent to calling `optional` with `identity`
-- | as a second parameter.
-- |
-- | Example:
-- | ```purescript
-- | optional' "arg"
-- | ```
optional'
  :: forall a b. StateMachine a InternalOptional b
  => String
  -> a
  -> ArgumentBuilder b
optional' n = optional n identity

nextState
  :: forall a b c. StateMachine a b c
  => b
  -> ArgumentType
  -> String
  -> (ArgumentFields -> ArgumentFields)
  -> a
  -> ArgumentBuilder c
nextState t a n f s = do
  tell (singleton $ buildEntry a n f)
  pure $ feed s t
