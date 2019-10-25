module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Aff (launchAff_)

import Test.Game (stepSpec, runGameSpec)

-- TODO: run is deprecated use runSpec instead
main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Game" do
    stepSpec
    runGameSpec
