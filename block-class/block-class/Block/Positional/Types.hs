module Block.Positional.Types where

import Essentials

import Integer (Positive)
import Block.End (End)

data Amount = Amount End Positive

{-| (Shortfall /n/) indicates that an operation which failed
    would require a block operand to have /n/ more items. -}
newtype Shortfall = Shortfall Positive
    deriving stock (Eq, Ord, Show)

newtype Prefix xs = Prefix xs
  deriving stock (Eq, Ord, Show, Functor)

newtype Suffix xs = Suffix xs
  deriving stock (Eq, Ord, Show, Functor)

data Split xs =
    SplitInsufficient Shortfall
  | Split (Prefix xs) (Suffix xs)
  deriving stock (Eq, Ord, Show, Functor)

data Drop xs =
    DropAll
  | DropInsufficient Shortfall
  | DropPart (Suffix xs)
  deriving stock (Eq, Ord, Show, Functor)

data Take xs =
    TakeAll
  | TakeInsufficient Shortfall
  | TakePart (Prefix xs)
  deriving stock (Eq, Ord, Show, Functor)
