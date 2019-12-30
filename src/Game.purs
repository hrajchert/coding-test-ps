module Game where

import Prelude

import Data.Either (Either(..))

import Capability.PrintOutput (class PrintOutput, print)
import Capability.PromptInput (class PromptInput, BoatInfo, promptBoatInfo)

type Monkeys = Int
type Wolfs = Int

type BoatPassengerCount = { monkeys :: Monkeys, wolfs :: Wolfs }

-- Convert the user input into something we can operate with (integers)
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


-- TODO: maybe make name more strict (initial | final)
newtype Shore = Shore {name :: String, monkeys :: Monkeys, wolfs :: Wolfs}
derive instance eqShore :: Eq Shore

instance shoreShow :: Show Shore where
  show (Shore {name, monkeys, wolfs}) =
    let
      repeat :: String -> Int -> String
      repeat str n | n > 0  = str <> repeat str (n-1)
      repeat str _ = ""
    in
      if name == "initial"
        then "🏝 " <> repeat "🐒 " monkeys <> " " <> repeat "🐺 " wolfs
        else repeat " 🐒" monkeys <> " " <> repeat " 🐺" wolfs <> " 🏝"

data State
  = BoatForward Shore Shore
  | BoatBackward Shore Shore

derive instance eqState :: Eq State

instance showState :: Show State where
  show (BoatForward from to) = show from <> "--🛶--->" <> show to
  show (BoatBackward from to) = show from <> "<--🛶---" <> show to

boatDirection :: State -> String
boatDirection (BoatForward _ _)  = "forward"
boatDirection (BoatBackward _ _) = "backward"

-- If the boat is moving backwards and there are no animals in the initial shore,
-- then we reached a final state
isFinalState :: State -> Boolean
isFinalState (BoatForward _ _) = false
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

-------------------------------------------------------------------------------
-- Game logic

-- Validate that in a shore there can't be more wolfs than monkeys
validateShore :: Shore -> Either InvalidMove Shore
validateShore (Shore s) =
  if s.monkeys > 0 && s.wolfs > s.monkeys
    then Left $ MoreWolfsThanMonkeys $ Shore s
    else Right $ Shore s


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

step :: State -> BoatInfo -> Either InvalidMove State
step state boatInfo =
  let
    passengerCount = countPassengers boatInfo
  in
    case state of
      (BoatForward initial final) -> do
        newInitialShore <- moveAnimalsToBoat initial passengerCount
        newFinalShore <- moveAnimalsToShore final passengerCount
        Right $ (BoatBackward newInitialShore newFinalShore)

      (BoatBackward initial final) -> do
        newFinalShore <- moveAnimalsToBoat final passengerCount
        newInitialShore <- moveAnimalsToShore initial passengerCount
        Right $ (BoatForward  newInitialShore newFinalShore)

runGame :: forall m
   . PrintOutput m
  => PromptInput m
  => State
  -> m State
runGame state = do
   -- In each step we start by showing the current state
   print $ show state
   -- We ask the user input and calculate the newState
   boatInfo <- promptBoatInfo $ boatDirection state
   case (step state boatInfo) of
     -- If there are some errors we print them and rerun in the same state
     Left err ->
       do
         print $ "We cant do that because: " <> show err
         runGame state
     -- If we finish we return the final step, if not we recurse
     Right s  ->
       if (isFinalState s)
         then do
           print "Congratulations! You have solved the puzzle"
           pure s
         else runGame s
