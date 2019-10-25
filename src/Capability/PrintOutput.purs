module Capability.PrintOutput
  ( class PrintOutput
  , print
  )
where

import Prelude

-- TypeClass that represents the capability to print messages to some output
class Monad m <= PrintOutput m where
    print :: String -> m Unit
