module Block.Class.Positional.Types
  (
    Shortfall (..), Take (..),
  )
  where

import Essentials

import Integer (Positive)

{-| (Shortfall /n/) indicates that an operation which failed
    would require a block operand to have /n/ more items. -}
newtype Shortfall = Shortfall Positive
    deriving stock (Eq, Ord, Show)

data Take xs =
    TakeInsufficient Shortfall
  | TakeAll
  | TakePart{ taken :: xs, remainder :: xs }
  deriving stock (Eq, Ord, Show, Functor)
