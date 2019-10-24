module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Main (State(..), Shore(..), step, InvalidMove(..))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Effect.Aff (launchAff_, Aff)

stepSpec :: Spec Unit
stepSpec = do
  describe "GameForward" do
    it "Should be possible to make a valid move forward"
      let
        initialState = BoatForward
          (Shore {name: "initial", monkeys: 3, wolfs: 3})
          (Shore {name: "final", monkeys: 0, wolfs: 0})

        boat = { first: "Monkey", second: "Wolf" }

        expectedState = BoatBackward
          (Shore {name: "initial", monkeys: 2, wolfs: 2})
          (Shore {name: "final", monkeys: 1, wolfs: 1})

      in
        step initialState boat `shouldEqual` Right expectedState

    it "Should not be possible to leave more wolves than monkeys in the initial shore moving forward"
      let
        initialState = BoatForward
          (Shore {name: "initial", monkeys: 3, wolfs: 3})
          (Shore {name: "final", monkeys: 0, wolfs: 0})

        boat = { first: "Monkey", second: "Monkey" }

        expectedFail = MoreWolfsThanMonkeys (Shore {name: "initial", monkeys: 1, wolfs: 3})
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "should not be possible to leave more wolves than monkeys in the final shore moving forward"
      let
        initialState = BoatForward
          (Shore {name: "initial", monkeys: 2, wolfs: 3})
          (Shore {name: "final", monkeys: 1, wolfs: 0})

        boat = { first: "Wolf", second: "Wolf" }

        expectedFail = MoreWolfsThanMonkeys (Shore {name: "final", monkeys: 1, wolfs: 2})
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "Should not be possible to move forward more monkeys than the ones we have"
      let
        initialState = BoatForward
          (Shore {name: "initial", monkeys: 0, wolfs: 3})
          (Shore {name: "final", monkeys: 3, wolfs: 0})

        boat = { first: "Monkey", second: "Wolf" }

        expectedFail = NotEnoughMonkeys
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "Should not be possible to move forward more wolves than the ones we have"
      let
        initialState = BoatForward
          (Shore {name: "initial", monkeys: 1, wolfs: 1})
          (Shore {name: "final", monkeys: 2, wolfs: 2})

        boat = { first: "Wolf", second: "Wolf" }

        expectedFail = NotEnoughWolfs
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "Should be possible to leave more wolves than monkeys in the final shore, if there are no monkeys"
      let
        initialState = BoatForward
          (Shore {name: "initial", monkeys: 3, wolfs: 3})
          (Shore {name: "final", monkeys: 0, wolfs: 0})

        boat = { first: "Wolf", second: "Wolf" }

        expectedState = BoatBackward
          (Shore {name: "initial", monkeys: 3, wolfs: 1})
          (Shore {name: "final", monkeys: 0, wolfs: 2})

      in
        step initialState boat `shouldEqual` Right expectedState

  describe "GameBackward" do
    it "Should be possible to make a valid move forward"
      let
        initialState = BoatBackward
          (Shore {name: "initial", monkeys: 2, wolfs: 2})
          (Shore {name: "final", monkeys: 1, wolfs: 1})

        boat = { first: "Monkey", second: "Empty" }

        expectedState = BoatForward
          (Shore {name: "initial", monkeys: 3, wolfs: 2})
          (Shore {name: "final", monkeys: 0, wolfs: 1})

      in
        step initialState boat `shouldEqual` Right expectedState

    it "Should not be possible to leave more wolves than monkeys in the initial shore moving backwards"
      let
        initialState = BoatBackward
          (Shore {name: "initial", monkeys: 1, wolfs: 1})
          (Shore {name: "final", monkeys: 2, wolfs: 2})

        boat = { first: "Wolf", second: "Empty" }

        expectedFail = MoreWolfsThanMonkeys (Shore {name: "initial", monkeys: 1, wolfs: 2})
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "Should not be possible to leave more wolves than monkeys in the final shore moving backwards"
      let
        initialState = BoatBackward
          (Shore {name: "initial", monkeys: 1, wolfs: 1})
          (Shore {name: "final", monkeys: 2, wolfs: 2})

        boat = { first: "Monkey", second: "Empty" }

        expectedFail = MoreWolfsThanMonkeys (Shore {name: "final", monkeys: 1, wolfs: 2})
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "Should not be possible to move backwards more monkeys than the ones we have"
      let
        initialState = BoatBackward
          (Shore {name: "initial", monkeys: 2, wolfs: 2})
          (Shore {name: "final", monkeys: 1, wolfs: 1})

        boat = { first: "Monkey", second: "Monkey" }

        expectedFail = NotEnoughMonkeys
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "Should not be possible to move backwards more wolves than the ones we have"
      let
        initialState = BoatBackward
          (Shore {name: "initial", monkeys: 3, wolfs: 2})
          (Shore {name: "final", monkeys: 0, wolfs: 1})

        boat = { first: "Wolf", second: "Wolf" }

        expectedFail = NotEnoughWolfs
      in
        step initialState boat `shouldEqual` Left expectedFail

    it "Should be possible to leave more wolves than monkeys in the initial shore, if there are no monkeys"
      let
        initialState = BoatBackward
          (Shore {name: "initial", monkeys: 0, wolfs: 2})
          (Shore {name: "final", monkeys: 3, wolfs: 1})

        boat = { first: "Wolf", second: "Empty" }

        expectedState = BoatForward
          (Shore {name: "initial", monkeys: 0, wolfs: 3})
          (Shore {name: "final", monkeys: 3, wolfs: 0})

      in
        step initialState boat `shouldEqual` Right expectedState

main :: Effect Unit
main = launchAff_ $ run [consoleReporter] do
  describe "Step" do
    stepSpec