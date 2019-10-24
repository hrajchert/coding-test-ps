module Enquirer
  ( prompt
  , promptMultiple
  , PromptOptions (..)
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried as FU
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)

data PromptOptions
  = SinglePrompt { name :: String, message :: String }
  | Select { name :: String, message :: String, choices :: Array String }

runPromiseAffE0 :: forall  o. FU.Fn0 (Effect (Promise o)) -> Aff o
runPromiseAffE0 f = Promise.toAffE $ FU.runFn0 f


runPromiseAffE1 :: forall a o. FU.Fn1 a (Effect (Promise o)) -> a -> Aff o
runPromiseAffE1 f a = Promise.toAffE $ FU.runFn1 f a

foreign import _prompt :: FU.Fn1 PromptOptions (Effect (Promise Foreign))

prompt :: PromptOptions -> Aff Foreign
prompt = runPromiseAffE1 _prompt

foreign import _promptMultiple :: FU.Fn1 (Array PromptOptions) (Effect (Promise Foreign))

promptMultiple :: Array PromptOptions -> Aff Foreign
promptMultiple = runPromiseAffE1 _promptMultiple