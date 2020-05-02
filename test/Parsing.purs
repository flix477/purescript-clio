module Test.Parsing (parsingSpec) where

import Prelude
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.Map (fromFoldable) as M
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Clio.Config (config', options, arguments, Config)
import Clio.Config.Option 
  ( Name(..)
  , value'
  )
import Clio.Config.Arguments (variadic', required')
import Clio.Parsing.Argument (parse, ArgsMap, ArgValue(..))
import Test.Parsing.Argument (argParsingSpec)
import Test.Parsing.Option (optParsingSpec)
import Test.Parsing.Parsed (parsedSpec)

testOption :: Name
testOption = Full 't' "test-option"

testConfig :: Config
testConfig = config' "test-config"
  [ options
    [ value' testOption ]
  , arguments
      $ variadic' "inputs"
    >=> required' "destination"
  ]

expectedArgs :: ArgsMap
expectedArgs = M.fromFoldable 
  [ Tuple "inputs" $ VariadicValue ("lol":"lol":Nil)
  , Tuple "destination" $ StringValue "req"
  ]

parsingSpec :: Spec Unit
parsingSpec = do
  describe "Parsing" do
    it "should parse correct arguments" do
       args `shouldEqual` expected
    argParsingSpec
    optParsingSpec
    parsedSpec
    where
      args = parse testConfig.arguments ("lol":"lol":"req":Nil)
      expected = Right expectedArgs
