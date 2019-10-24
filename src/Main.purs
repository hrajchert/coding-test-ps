module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Foreign (Foreign)
import Enquirer (PromptOptions(..), promptMultiple)
import Simple.JSON (read, class ReadForeign)

parseResponse :: forall a.  ReadForeign a => Foreign -> Aff a
parseResponse f = case read f of
  Right r -> pure r
  Left e -> throwError $ error "Could not interpret value"


type Monkeys = Int

type Wolfs = Int

-- TODO: maybe make name more strict (initial | final)
newtype Shore = Shore {name :: String, monkeys :: Monkeys, wolfs :: Wolfs}
derive instance eqShore :: Eq Shore

instance shoreShow :: Show Shore where
  show (Shore {name, monkeys, wolfs}) = "the " <> name <> " shore has " <> show monkeys <> " monkeys and " <> show wolfs <> " wolfs"


data State
  = BoatForward Shore Shore
  | BoatBackward Shore Shore

derive instance eqState :: Eq State

instance showState :: Show State where
  show (BoatForward from to) = "The boat is moving forwards, " <> show from <> " and " <> show to
  show (BoatBackward from to) = "The boat is moving backwards, " <> show from <> " and " <> show to

boatDirection :: State -> String
boatDirection (BoatForward _ _)  = "forward"
boatDirection (BoatBackward _ _) = "backward"

isFinalState :: State -> Boolean
isFinalState (BoatForward _ _) = false
-- If the boat is moving backwards and there are no animals in the initial shore, then we reached a final state
isFinalState (BoatBackward (Shore {monkeys, wolfs}) _) = monkeys == 0 && wolfs == 0

data InvalidMove
  = NotEnoughMonkeys
  | NotEnoughWolfs
  | MoreWolfsThanMonkeys Shore

derive instance eqInvalidMove :: Eq InvalidMove

instance showInvalidMove :: Show InvalidMove where
  show NotEnoughMonkeys = "There aren't that many monkeys to move to the other shore"
  show NotEnoughWolfs = "There aren't that many wolfs to move to the other shore"
  show (MoreWolfsThanMonkeys (Shore shore)) = "There are " <> show shore.wolfs <> " wolfs and " <> show shore.monkeys <> " monkeys in the " <> shore.name <> " shore, tragedies may happen"



step :: State -> BoatInfo -> Either InvalidMove State
step (BoatForward initial final) boat = do
  let
    count = countPassengers boat
  newInitialShore <- moveAnimalsToBoat initial count
  newFinalShore <- moveAnimalsToShore final count
  Right $ (BoatBackward newInitialShore newFinalShore)
step (BoatBackward initial final) boat = do
  let
    count = countPassengers boat
  newFinalShore <- moveAnimalsToBoat final count
  newInitialShore <- moveAnimalsToShore initial count
  Right $ (BoatForward  newInitialShore newFinalShore)

-- Remove the animals from the shore, validating that there are enough animals and that the shore is still valid after
moveAnimalsToBoat :: Shore -> BoatPassengerCount -> Either InvalidMove Shore
moveAnimalsToBoat (Shore from) count =
  if count.monkeys > from.monkeys
      then Left NotEnoughMonkeys
      else if count.wolfs > from.wolfs
        then Left NotEnoughWolfs
        else validateShore (Shore {name: from.name, monkeys: from.monkeys - count.monkeys, wolfs: from.wolfs - count.wolfs})

-- Insert animals into the shore, validating that the result is still valid
moveAnimalsToShore :: Shore -> BoatPassengerCount -> Either InvalidMove Shore
moveAnimalsToShore (Shore to) count = validateShore (Shore {name: to.name, monkeys: to.monkeys + count.monkeys, wolfs: to.wolfs + count.wolfs})

-- Validate that in a shore there can't be more wolfs than monkeys
validateShore :: Shore -> Either InvalidMove Shore
validateShore (Shore s) =
  if s.monkeys > 0 && s.wolfs > s.monkeys
    then Left $ MoreWolfsThanMonkeys $ Shore s
    else Right $ Shore s


-- TODO: Maybe strict first and second to use a particular string
type BoatInfo = { first :: String, second :: String }

promptBoatInfo :: String -> Aff BoatInfo
promptBoatInfo direction = parseResponse =<< promptMultiple
  [ (Select { name: "first", message: "What is the first animal to row " <> direction, choices: ["Monkey", "Wolf"] })
  , (Select { name: "second", message: "What is the second animal to row " <> direction, choices: ["Monkey", "Wolf", "Empty"] })
  ]

countPassengers :: BoatInfo -> BoatPassengerCount
countPassengers {first, second} =
  let
    countMonkeys "Monkey" = 1
    countMonkeys _        = 0
    countWolf "Wolf"  = 1
    countWolf _       = 0
  in
    { monkeys:  (countMonkeys first + countMonkeys second)
    , wolfs:  (countWolf first + countWolf second)
    }

type BoatPassengerCount = { monkeys :: Monkeys, wolfs :: Wolfs }

run :: Aff State
run = _run (BoatForward (Shore {name: "initial", monkeys: 3, wolfs: 3}) (Shore {name: "final", monkeys: 0, wolfs: 0})) where
  log' = liftEffect <<< log

  _run :: State -> Aff State
  _run state = do
    -- In each step we start by showing the current state
    log' $ show state
    -- We ask the user input and calculate the newState
    boatInfo <- promptBoatInfo $ boatDirection state
    case (step state boatInfo) of
      -- If there are some errors we print them and rerun in the same state
      Left err -> do
        log' $ "We cant do that because: " <> show err
        _run state
      -- If we finish we return the final step, if not we recurse
      Right s  -> if (isFinalState s)
        then do
          log' "Congratulations! You have solved the puzzle"
          pure s
        else _run s

main :: Effect Unit
main = launchAff_ run
