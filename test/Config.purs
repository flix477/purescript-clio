module Test.Config (configSpec) where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple (fst)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec (Spec, describe, it)

import Clio.Config (config', options, arguments, Config)
import Clio.Config.Option (Name(..), value')
import Clio.Config.Arguments (variadic', required')
import Test.Config.Help (helpSpec)

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

configSpec :: Spec Unit
configSpec = do
  describe "Config" do
    it "should build correct arguments" do
      (fst <$> testConfig.arguments) `shouldEqual` ("inputs":"destination":Nil)
    helpSpec
