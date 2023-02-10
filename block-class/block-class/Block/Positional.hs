module Block.Positional
  (
    {- * Class -} Positional (..),
    {- * Types -} Split (..), Shortfall (..), SplitResult (..), Truncate (..), TruncateResult (..),
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

    split :: Split -> xs -> SplitResult xs

    truncate :: Truncate -> xs -> TruncateResult xs

instance Positional (NonEmpty xs) where

    length = Positive.length

data Split = Split End Positive

data Truncate = Drop End Positive | Take End Positive

{-| (Shortfall /n/) indicates that an operation which failed
    would require a block operand to have /n/ more items. -}
newtype Shortfall = Shortfall Positive
    deriving stock (Eq, Ord, Show)

data SplitResult xs =
    SplitFailure Shortfall
  | SplitResult xs xs
  deriving stock (Eq, Ord, Show, Functor)

data TruncateResult xs =
    TruncateHasNoEffect
  | TruncateFailure Shortfall
  | TruncateResult xs
  deriving stock (Eq, Ord, Show, Functor)
