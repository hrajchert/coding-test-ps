module Capability.PromptInput where

import Prelude

-- TODO: Maybe strict first and second to use a particular string
type BoatInfo = { first :: String, second :: String }

-- TODO: maybe refactor to Maybe to indicate user cancellation
class Monad m <= PromptInput m where
  promptBoatInfo :: String -> m BoatInfo


