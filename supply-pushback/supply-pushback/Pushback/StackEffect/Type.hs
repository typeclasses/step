module Pushback.StackEffect.Type
  (
    {- * Type -} StackEffect (..),
  )
  where

import Essentials

{-| Effectful push and pop operations for a stack -}
data StackEffect action item = StackEffect
    { push :: item -> action ()
    , pop :: action (Maybe item)
    }
