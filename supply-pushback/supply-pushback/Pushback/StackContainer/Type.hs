module Pushback.StackContainer.Type
  (
    {- * Type -} StackContainer (..),
  )
  where

import Essentials

{-| Push and pop operations for a stack data structure -}
data StackContainer container item = StackContainer
    { push :: item -> container -> container
    , pop :: container -> Maybe (item, container)
    }
