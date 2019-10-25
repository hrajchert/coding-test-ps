module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Game (State(..), Shore(..), runGame)
import AppM (runAppM)



main :: Effect Unit
main = (launchAff_ <<< runAppM) $ runGame initialState where
  initialState = (BoatForward (Shore {name: "initial", monkeys: 3, wolfs: 3}) (Shore {name: "final", monkeys: 0, wolfs: 0}))
