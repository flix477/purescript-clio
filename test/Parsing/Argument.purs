module Test.Parsing.Argument (argParsingSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List ((:), List(..), take)
import Data.List (singleton) as L
import Data.Either (Either(..))
import Data.Map (singleton, fromFoldable, empty) as M
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Clio.Config.Argument (ArgumentType(..), ArgumentEntry)
import Clio.Parsing.Argument
  ( ArgValue(..)
  , parse
  , ArgParseError(..)
  )

required :: String -> ArgumentEntry
required n = Tuple n
  { description: Nothing, argumentType: Required }

optional :: String -> ArgumentEntry
optional n = Tuple n
  { description: Nothing, argumentType: Optional }

variadic :: String -> ArgumentEntry
variadic n = Tuple n
  { description: Nothing, argumentType: Variadic }

argParsingSpec :: Spec Unit
argParsingSpec = do
  describe "Argument" do
    it "should return empty map if there are no arguments" do
      parse Nil Nil `shouldEqual` pure M.empty

    it "should parse required argument" do
      let argName = "arg0"
      let value = "value"
      let values = L.singleton value
      let args = L.singleton $ required argName
      let expected = M.singleton argName (StringValue value)
      parse args values `shouldEqual` pure expected

    it "should fail on missing required argument" do
      let argName = "arg0"
      let expected = MissingArgument argName
      parse (L.singleton $ required argName) Nil `shouldEqual` Left expected

    it "should parse optional argument" do
      let argName = "arg0"
      let value = "value"
      let values = L.singleton value
      let args = L.singleton $ optional argName
      let expected = M.singleton argName (StringValue value)
      parse args values `shouldEqual` pure expected

    it "should skip missing optional argument" do
      let args = L.singleton $ optional "arg0"
      let expected = M.empty
      parse args Nil `shouldEqual` pure expected

    it "should parse variadic argument" do
      let argName = "arg0"
      let values = ("value1" : "value2" : Nil)
      let args = L.singleton $ variadic argName
      let expected = M.singleton argName (VariadicValue values)
      parse args values `shouldEqual` pure expected

    it "should parse variadic argument with trailing required argument" do
      let argName = "arg0"
      let requiredName = "arg1"
      let requiredValue = "value3"
      let values = ("value1" : "value2" : requiredValue : Nil)
      let expectedVariadicValues = take 2 values
      let args = (variadic argName : required requiredName : Nil)
      let expected = M.fromFoldable (Tuple argName (VariadicValue expectedVariadicValues) : Tuple requiredName (StringValue requiredValue) : Nil)
      parse args values `shouldEqual` pure expected

    it "should assign Nil value to missing variadic argument" do
      let argName = "arg0"
      let args = L.singleton $ variadic argName
      let expected = M.singleton argName (VariadicValue Nil)
      parse args Nil `shouldEqual` pure expected

    it "should fail on extraneous argument" do
      let arg = "arg0"
      let expected = InvalidArgument arg
      parse Nil (L.singleton arg) `shouldEqual` Left expected
