module Block.Positional
  (
    {- * Class -} Positional (..),
    {- * Types -} Amount (..), Shortfall (..), Split (..), Truncate (..), TruncateResult (..),
  )
  where

import Essentials

import Block.Singleton.Class

import Integer (Positive)
import Data.List.NonEmpty (NonEmpty (..))
import Block.End (End)

import qualified Integer.Positive as Positive

class (Singleton xs) => Positional xs where

    length :: xs -> Positive

    split :: Amount -> xs -> Split xs

    truncate :: Truncate -> xs -> TruncateResult xs

instance Positional (NonEmpty xs) where

    length = Positive.length

data Amount = Amount End Positive

data Truncate = Drop End Positive | Take End Positive

{-| (Shortfall /n/) indicates that an operation which failed
    would require a block operand to have /n/ more items. -}
newtype Shortfall = Shortfall Positive
    deriving stock (Eq, Ord, Show)

data Split xs =
    SplitInsufficient Shortfall
  | Split xs xs
  deriving stock (Eq, Ord, Show, Functor)

data TruncateResult xs =
    TruncateHasNoEffect
  | TruncateFailure Shortfall
  | TruncateResult xs
  deriving stock (Eq, Ord, Show, Functor)
