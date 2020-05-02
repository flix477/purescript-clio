module Clio.Parsing.Argument
  ( parse
  , ArgValue(..)
  , ArgsMap
  , ArgParseError(..)
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.List (List(..), (:), snoc)
import Data.Map as M
import Data.Map (Map)

import Clio.Config.Arguments (Arguments)
import Clio.Config.Argument (ArgumentEntry, ArgumentType(..))

type ArgsMap = Map String ArgValue
type ArgEntry = Tuple String ArgValue
type ParseResult = Either ArgParseError ArgsMap

type InternalArgumentEntry = Tuple String ArgumentType
type InternalArguments = List InternalArgumentEntry

data ArgParseError
  = MissingArgument String
  | InvalidArgument String

instance showArgParseError :: Show ArgParseError where
  show (MissingArgument x) = show x
  show (InvalidArgument x) = show x

instance eqArgParseError :: Eq ArgParseError where
  eq (MissingArgument a) (MissingArgument b) = eq a b
  eq (InvalidArgument a) (InvalidArgument b) = eq a b
  eq _ _ = false

data ArgValue
  = StringValue String
  | VariadicValue (List String)

instance showArgValue :: Show ArgValue where
  show (StringValue x) = show x
  show (VariadicValue xs) = show xs

instance eqArgValue :: Eq ArgValue where
  eq (StringValue x) (StringValue y) = eq x y
  eq (VariadicValue xs) (VariadicValue ys) = eq xs ys
  eq _ _ = false

type ParsedArg = Tuple String ArgValue

addString :: String -> String -> ArgsMap -> ArgsMap
addString n v m = M.insert n (StringValue v) m

addVariadic :: String -> List String -> ArgsMap -> ArgsMap
addVariadic n v m = M.insert n (VariadicValue v) m

toInternal :: ArgumentEntry -> Tuple String ArgumentType
toInternal (Tuple n a) = Tuple n a.argumentType

parse :: Arguments -> List String -> Either ArgParseError ArgsMap
parse args values = parse' (map toInternal args) values M.empty

parse' :: InternalArguments -> List String -> ArgsMap -> ParseResult
parse' Nil Nil m = pure m
parse' Nil (x:_) _ = Left $ InvalidArgument x
parse' ((Tuple n Required):_) Nil _ = Left $ MissingArgument n
parse' ((Tuple n Optional):xs) Nil m = pure m
parse' ((Tuple n Variadic):xs) v m = parseVariadic v Nil f
  where f values variadic = parse' xs values $ addVariadic n variadic m
parse' ((Tuple n Required):xs) (v:vs) m = parse' xs vs $ addString n v m
parse' ((Tuple n Optional):xs) values@(v:vs) m
  | Right m' <- parse' xs vs $ addString n v m = pure m'
  | otherwise = parse' xs values m

-- TODO: check if we can use something that appends O(1)
-- and make this faster by using trailing required count
-- which can be put in a newtype Arguments
parseVariadic
  :: List String
  -> List String
  -> (List String -> List String -> ParseResult)
  -> ParseResult
parseVariadic Nil variadic f = f Nil variadic
parseVariadic values@(v:vs) variadic f = 
  parseVariadic vs (snoc variadic v) f
  `orElse`
  \_ -> f values variadic

orElse :: forall e a. Either e a -> (Unit -> Either e a) -> Either e a
orElse (Left _) f = f unit
orElse x _ = x
