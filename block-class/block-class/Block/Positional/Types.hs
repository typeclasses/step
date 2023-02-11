module Block.Positional.Types
  (
    Shortfall (..), Span (..),
  )
  where

import Essentials

import Integer (Positive)

{-| (Shortfall /n/) indicates that an operation which failed
    would require a block operand to have /n/ more items. -}
newtype Shortfall = Shortfall Positive
    deriving stock (Eq, Ord, Show)

data Span xs =
    SpanInsufficient Shortfall
  | Span{ spannedPart :: xs, remainder :: Maybe xs }
  deriving stock (Eq, Ord, Show, Functor)
