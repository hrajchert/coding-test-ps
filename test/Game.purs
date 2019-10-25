module Test.Game where

import Prelude

import Capability.PrintOutput (class PrintOutput)
import Capability.PromptInput (class PromptInput, BoatInfo)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.State.Class (class MonadState)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Newtype (class Newtype, unwrap)
import Game (State(..), Shore(..), step, InvalidMove(..), runGame)
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Data.Tuple (Tuple(..))
import Data.Array (unsafeIndex)
import Partial.Unsafe (unsafePartial)

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

newtype TestSate = TestState (Tuple (Array BoatInfo) Int)
newtype TestM a = TestM (StateT TestSate Identity a)

runTestM :: forall a. Array (Array String) -> TestM a -> a
runTestM inputs (TestM stateM) =
  let
    boatInfos = (\input -> { first: (unsafePartial $ unsafeIndex input 0), second: (unsafePartial $ unsafeIndex input 1)}) <$> inputs
    evalState = evalStateT stateM $ TestState $ Tuple boatInfos 0
  in
    unwrap evalState


-- Derive the instances that would make TestM an actual Monad
derive instance newtypeTestM :: Newtype (TestM a) _
derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM

-- Derive instances to work with the underlying monad transformers
derive newtype instance monadStateTestM :: MonadState TestSate TestM

-- Now we can create the mock implementation for the abstract capabilities.
instance printOutputTestM :: PrintOutput TestM where
  print msg = pure unit

-- TODO: Do not run with this as it will do a infinite loop
instance promptInputTestM :: PromptInput TestM where
  promptBoatInfo :: String -> TestM BoatInfo
  promptBoatInfo direction = do
    (TestState (Tuple inputs index)) <- get
    -- TODO: We should either change the promptInput capability to return a Maybe (m BoatInfo)
    --       or add a MonadError to TestM with a special error if the index gets out of bound
    put (TestState (Tuple inputs (index + 1)))
    pure $ unsafePartial $ unsafeIndex inputs index
  -- promptBoatInfo direction = pure { first: "Monkey", second: "Wolf"}


runGameSpec :: Spec Unit
runGameSpec = do
  describe "runGame" do
    it "Should be possible to win"
      let
        initialState = BoatForward
          (Shore {name: "initial", monkeys: 3, wolfs: 3})
          (Shore {name: "final", monkeys: 0, wolfs: 0})

        input =
          -- 1 wolf and 1 monkey row there, monkey rows back.
          [ ["Wolf", "Monkey"]
          , ["Monkey", "Empty"]
            -- 2 wolves row there, 1 wolf rows back.
          , ["Wolf", "Wolf"]
          , ["Wolf", "Empty"]
            -- 2 monkeys row there, 1 monkey and 1 wolf rows back.
          , ["Monkey", "Monkey"]
          , ["Monkey", "Wolf"]
            -- 2 monkeys row there, 1 wolf rows back.
          , ["Monkey", "Monkey"]
          , ["Wolf", "Empty"]
            -- This one wolf takes the remaining wolves to the other side.
          , ["Wolf", "Wolf"]
          , ["Wolf", "Empty"]
          , ["Wolf", "Wolf"]
          ]

        expectedState = BoatBackward
          (Shore {name: "initial", monkeys: 0, wolfs: 0})
          (Shore {name: "final", monkeys: 3, wolfs: 3})
      in
        (runTestM input $ runGame initialState) `shouldEqual` expectedState
