module AppM
  ( runAppM
  , AppM
  )
where

import Prelude

import Capability.PrintOutput (class PrintOutput)
import Capability.PromptInput (class PromptInput)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Enquirer (PromptOptions(..), promptMultiple)
import Foreign (Foreign)
import Simple.JSON (read, class ReadForeign)


newtype AppM a = AppM (Aff a)

runAppM :: forall a. AppM a -> Aff a
runAppM (AppM a) = a

-- Derive the instances that would make AppM an actual Monad
derive instance newtypeAppM :: Newtype (AppM a) _
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM

-- Derive instances for Aff and Effect so we can run any arbitrary effect here.
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM


-- Now we can create the concrete implementation for the abstract capabilities.

instance printOutputAppM :: PrintOutput AppM where
  print = liftEffect <<< log


parseResponse :: forall a.  ReadForeign a => Foreign -> Aff a
parseResponse f = case read f of
    Right r -> pure r
    Left e -> throwError $ error "Could not interpret value"



instance promptInputAppM :: PromptInput AppM where
  promptBoatInfo direction = liftAff $ parseResponse =<< promptMultiple
    [ (Select { name: "first", message: "What is the first animal to row " <> direction, choices: ["Monkey", "Wolf"] })
    , (Select { name: "second", message: "What is the second animal to row " <> direction, choices: ["Monkey", "Wolf", "Empty"] })
    ]