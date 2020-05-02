module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec (describe)

import Test.Config (configSpec)
import Test.Parsing (parsingSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Clio" do
    configSpec
    parsingSpec
