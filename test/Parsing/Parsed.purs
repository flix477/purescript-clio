module Test.Parsing.Parsed (parsedSpec) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Map (empty, singleton) as M
import Data.List (List(..))
import Data.List (singleton) as L
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Clio.Config.Option (Name(..))
import Clio.Parsing.Argument (ArgsMap, ArgValue(..))
import Clio.Parsing.Option (OptionsMap)
import Clio.Parsing.Parsed
  ( Parsed(..)
  , hasOption
  , getOption
  , getArgument
  , getVariadicArgument
  )

parsedOptions :: OptionsMap -> Parsed
parsedOptions o = Parsed { options: o, arguments: M.empty }

parsedArguments :: ArgsMap -> Parsed
parsedArguments a = Parsed { arguments: a, options: M.empty }

parsedSpec :: Spec Unit
parsedSpec = do
  describe "Parsed" do
    describe "hasOption" do
      it "should return true when the option is present" do
        let name = Short 'a'
        let p = parsedOptions $ M.singleton name Nothing
        hasOption p name `shouldEqual` true
      it "should return false when the option is absent" do
        let p = parsedOptions $ M.singleton (Short 'b') Nothing
        hasOption p (Short 'a') `shouldEqual` false
      it "should return false when there are no options" do
        let p = parsedOptions $ M.empty
        hasOption p (Short 'a') `shouldEqual` false

    describe "getOption" do
      it "should return the value when the option is present and has a value" do
        let name = Short 'a'
        let value = Just "value"
        let p = parsedOptions $ M.singleton name value
        getOption p name `shouldEqual` value
      it "should return Nothing when the option is present and has no value" do
        let name = Short 'a'
        let p = parsedOptions $ M.singleton name Nothing
        getOption p name `shouldEqual` Nothing
      it "should return Nothing when the option is absent" do
        let p = parsedOptions (M.singleton (Short 'b') (Just "value"))
        getOption p (Short 'a') `shouldEqual` Nothing
      it "should return Nothing when there are no options" do
        let p = parsedOptions $ M.empty
        getOption p (Short 'a') `shouldEqual` Nothing

    describe "getArgument" do
      it "should return the value when the argument is present and isn't variadic" do
        let name = "arg"
        let value = "value"
        let p = parsedArguments (M.singleton name $ StringValue value)
        getArgument p name `shouldEqual` pure value
      it "should return Nothing when the argument is present and is variadic" do
        let name = "arg"
        let p = parsedArguments (M.singleton name $ VariadicValue Nil)
        getArgument p name `shouldEqual` Nothing
      it "should return Nothing when the argument is absent" do
        let p = parsedArguments (M.singleton "arg1" $ StringValue "value")
        getArgument p "arg" `shouldEqual` Nothing
      it "should return Nothing when there are no arguments" do
        let p = parsedArguments $ M.empty
        getArgument p "arg" `shouldEqual` Nothing

    describe "getVariadicArgument" do
      it "should return the values when the argument is present and is variadic" do
        let name = "arg"
        let values = L.singleton "value"
        let p = parsedArguments (M.singleton name $ VariadicValue values)
        getVariadicArgument p name `shouldEqual` pure values
      it "should return Nothing when the argument is present and isn't variadic" do
        let name = "arg"
        let p = parsedArguments (M.singleton name $ StringValue "value")
        getVariadicArgument p name `shouldEqual` Nothing
      it "should return Nothing when the argument is absent" do
        let p = parsedArguments (M.singleton "arg1" (VariadicValue $ L.singleton "value"))
        getVariadicArgument p "arg" `shouldEqual` Nothing
      it "should return Nothing when there are no arguments" do
        let p = parsedArguments $ M.empty
        getVariadicArgument p "arg" `shouldEqual` Nothing
