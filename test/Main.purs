module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Effect.Aff (launchAff_)

import Test.Game (stepSpec)

-- TODO: run is deprecated use runSpec instead
main :: Effect Unit
main = launchAff_ $ run [consoleReporter] do
  describe "Step" do
    stepSpec