module Test.Parsing.Option (optParsingSpec) where

import Prelude

import Data.Map (singleton, empty) as M
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.List ((:), List(..), singleton)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Clio.Parsing.Option
  ( parseOptions
  , OptionParseError(..)
  , ParsedKey(..)
  )
import Clio.Config.Option
  ( Name(..)
  , switch'
  , value'
  , OptionEntry
  )

singleOption :: Name -> List OptionEntry
singleOption = singleton <<< switch'

optParsingSpec :: Spec Unit
optParsingSpec = do
  describe "Option" do
    it "should return empty map on empty options" do
      parseOptions Nil Nil `shouldEqual` pure M.empty

    it "should return empty map on empty input" do
      let name = Short 'o'
      let options = singleOption name
      parseOptions options Nil `shouldEqual` pure M.empty

    it "should match short name" do
      let name = Short 'o'
      let options = singleOption name
      let input = ("-o" : Nil)
      let expected = M.singleton name Nothing
      parseOptions options input `shouldEqual` pure expected

    it "should not match short name on double dashes" do
      let name = Short 'o'
      let options = singleOption name
      let input = ("--o" : Nil)
      let expected = InvalidOption $ ParsedLong "o"
      parseOptions options input `shouldEqual` Left expected

    it "should match long name" do
      let name = Long "option"
      let options = singleOption name
      let input = ("--option" : Nil)
      let expected = M.singleton name Nothing
      parseOptions options input `shouldEqual` pure expected

    it "should not match long name on single dash" do
      let name = Long "option"
      let options = singleOption name
      let input = ("-option" : Nil)
      parseOptions options input `shouldEqual` pure M.empty

    it "should match full name by short flag" do
      let name = Full 'o' "option"
      let options = singleOption name
      let input = ("-o" : Nil)
      let expected = M.singleton name Nothing
      parseOptions options input `shouldEqual` pure expected

    it "should match full name by long flag" do
      let name = Full 'o' "option"
      let options = singleOption name
      let input = ("--option" : Nil)
      let expected = M.singleton name Nothing
      parseOptions options input `shouldEqual` pure expected

    it "should match option value" do
      let name = Short 'o'
      let value = "value"
      let options = singleton $ value' name
      let input = ("-o" : value : Nil)
      let expected = M.singleton name $ Just value
      parseOptions options input `shouldEqual` pure expected

    it "should fail on missing option value" do
      let name = Short 'o'
      let option = value' name
      let options = singleton option
      let input = ("-o" : Nil)
      let expected = MissingValue option
      parseOptions options input `shouldEqual` Left expected

    it "should fail on invalid option" do
      let options = singleOption (Short 'c')
      let input = ("-o" : Nil)
      let expected = InvalidOption (ParsedShort 'o')
      parseOptions options input `shouldEqual` Left expected
