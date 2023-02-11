module Block.Positional
  (
    {- * Class -} Positional (..),
    {- * Types -} Split (..), Shortfall (..), SplitResult (..), Truncate (..), TruncateResult (..),
    {- * Utilities -} flipSplit,
  )
  where

import Essentials

import Block.Singleton.Class

import Integer (Positive)
import Data.List.NonEmpty (NonEmpty (..))
import Block.End (End)
import Data.Either (Either)
import Prelude ((+))

import qualified Data.Either as Either
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed
import qualified Block.End as End

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

{-| Produces a 'Split' that has the same effect as the original
    'Split' but is expressed in the opposite direction -}
flipSplit :: Positional xs => xs -> Split -> Either Shortfall Split
flipSplit xs (Split e n) = case Positive.subtract (length xs) n of
    Signed.Zero -> Either.Left (Shortfall 1)
    Signed.Minus s -> Either.Left (Shortfall (s + 1))
    Signed.Plus r -> Either.Right (Split (End.opposite e) r)
