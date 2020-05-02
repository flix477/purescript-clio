module Test.Config.Help (helpSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec (Spec, describe, it)

import Clio.Config.Help
  ( usage
  , formatArgumentName
  , title
  , option
  , argument
  , help
  ) as H
import Clio.Config (Config, options, arguments, config, description)
import Clio.Config.Arguments (variadic, variadic', required')
import Clio.Config.Argument (ArgumentType(..), ArgumentEntry)
import Clio.Config.Argument (description) as A
import Clio.Config.Option 
  ( switch
  , switch'
  , value
  , value'
  , Name(..)
  , OptionEntry
  )
import Clio.Config.Option (description) as O

requiredEntry :: String -> ArgumentEntry
requiredEntry n = Tuple n
  { description: Nothing, argumentType: Required }

optionalEntry :: String -> ArgumentEntry
optionalEntry n = Tuple n
  { description: Nothing, argumentType: Optional }

variadicEntry :: String -> ArgumentEntry
variadicEntry n = Tuple n
  { description: Nothing, argumentType: Variadic }

switchAll :: Name
switchAll = Short 'a'

switchLong :: Name
switchLong = Short 'l'

switchRecursive :: Name
switchRecursive = Short 'R'

configWithOptions :: Config
configWithOptions =
  config "test"
  $ options
      [ switch' switchAll
      , switch' switchLong
      , value' switchRecursive
      ]

configWithoutOptions :: Config
configWithoutOptions = config "test" identity

configWithDescription :: Config
configWithDescription = config "test" $ description "description"

configWithArguments :: Config
configWithArguments = config "test"
  $ arguments (variadic' "argVar" >=> required' "arg")

fullConfig :: Config
fullConfig =
  config "test" $
    (options
      [ switch' $ Short 'a'
      , switch (Long "long")
        $ O.description "long description"
      , value (Full 'C' "custom")
        $ O.description "custom description"
      ]
    ) <<<
    (arguments
        $ (variadic "args" $ A.description "args description")
      >=> required' "arg"
    )

fullConfigExpected :: String
fullConfigExpected = """test
Usage: test [options] [...args] arg
Options:
  -a
  --long              long description
  -C, --custom VALUE  custom description
Arguments:
  [...args]           args description
  arg"""

spacedOption :: Int -> OptionEntry -> String
spacedOption s = uncurry $ H.option s

zeroSpacedOption :: OptionEntry -> String
zeroSpacedOption = spacedOption 0

helpSpec :: Spec Unit
helpSpec = do
  describe "Help" do
    it "should display config correctly" do
      (H.help fullConfig) `shouldEqual` fullConfigExpected

    describe "title" do
      it "should display title without description correctly" do
        H.title configWithOptions `shouldEqual` "test"
      it "should display title with description correctly" do
        H.title configWithDescription `shouldEqual` "test: description"

    describe "formatArgumentName" do
      it "should display usage required arguments correctly" do
        let arg = requiredEntry "arg"
        H.formatArgumentName arg `shouldEqual` "arg"
      it "should display usage optional arguments correctly" do
        let arg = optionalEntry "arg"
        H.formatArgumentName arg `shouldEqual` "[arg]"
      it "should display usage variadic arguments correctly" do
        let arg = variadicEntry "args"
        H.formatArgumentName arg `shouldEqual` "[...args]"

    describe "usage" do
      it "should display correctly with options" do
        (H.usage configWithOptions) `shouldEqual` "Usage: test [options]"
      it "should display correctly with no options" do
        (H.usage configWithoutOptions) `shouldEqual` "Usage: test"
      it "should display correctly with arguments" do
        (H.usage configWithArguments) `shouldEqual` "Usage: test [...argVar] arg"

    describe "option" do
      it "should display short option correctly" do
        let opt = switch' $ Short 'a'
        (zeroSpacedOption opt) `shouldEqual` "-a"
      it "should display long option correctly" do
        let opt = switch' $ Long "arg"
        (zeroSpacedOption opt) `shouldEqual` "--arg"
      it "should display full option correctly" do
        let opt = switch' $ Full 'a' "arg"
        (zeroSpacedOption opt) `shouldEqual` "-a, --arg"
      it "should display option with value correctly" do
        let opt = value' $ Short 'a'
        (zeroSpacedOption opt) `shouldEqual` "-a VALUE"
      it "should display description correctly" do
        let opt = switch (Short 'a') $ O.description "A description"
        (zeroSpacedOption opt) `shouldEqual` "-a  A description"
      it "should pad description correctly" do
        let opt = switch (Short 'a') $ O.description "A description"
        (spacedOption 3 opt) `shouldEqual` "-a   A description"

    describe "argument" do
      it "should display argument correctly" do
        (H.argument 0 $ requiredEntry "arg") `shouldEqual` "arg"
      it "should display argument with description correctly" do
        let entry = Tuple "arg" { description: Just "description", argumentType: Optional }
        (H.argument 0 entry) `shouldEqual` "[arg]  description"
      it "should pad argument description correctly" do
        let entry = Tuple "args" { description: Just "description", argumentType: Variadic }
        (H.argument 10 entry) `shouldEqual` "[...args]   description"
