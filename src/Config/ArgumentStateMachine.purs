module Clio.Config.ArgumentStateMachine
  ( class StateMachine
  , feed
  , StartState(..)
  , VariadicState(..)
  , OptionalState(..)
  , RequiredState(..)
  , TailRequiredState(..)
  , InternalRequired(..)
  , InternalOptional(..)
  , InternalVariadic(..)
  ) where

data VariadicState = VariadicState
data OptionalState = OptionalState
data RequiredState = RequiredState
data TailRequiredState = TailRequiredState
data StartState = StartState

data InternalVariadic = InternalVariadic
data InternalOptional = InternalOptional
data InternalRequired = InternalRequired

class StateMachine a i b | i -> b where
  feed :: a -> i -> b

instance startToVariadic
  :: StateMachine StartState InternalVariadic VariadicState where
  feed s a = VariadicState

instance startToOptional
  :: StateMachine StartState InternalOptional OptionalState where
  feed s a = OptionalState

instance startToRequired
  :: StateMachine StartState InternalRequired StartState where
  feed s a = StartState

instance optionalToOptional
  :: StateMachine OptionalState InternalOptional OptionalState where
  feed s a = OptionalState

instance optionalToRequired
  :: StateMachine OptionalState InternalRequired TailRequiredState where
  feed s a = TailRequiredState

instance variadicToRequired
  :: StateMachine VariadicState InternalRequired TailRequiredState where
  feed s a = TailRequiredState

instance tailArgToRequired
  :: StateMachine TailRequiredState InternalRequired TailRequiredState where
  feed s a = TailRequiredState
