module Block.Positional
  (
    {- * Class -} Positional (..),
    {- * Types -} Split (..), SplitResult (..), Shortfall (..),
        Truncate (..), TruncateResult (..),
    {- * Utilities -} flipSplit, flipTruncate,
  )
  where

import Essentials

import Block.Singleton.Class

import Integer (Positive)
import Data.List.NonEmpty (NonEmpty (..))
import Block.End (End)
import Data.Either (Either)
import Prelude ((+))
import Block.TakeOrDrop (TakeOrDrop)

import qualified Data.Either as Either
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed
import qualified Block.End as End
import qualified Block.TakeOrDrop as TakeOrDrop

class (Singleton xs) => Positional xs where

    length :: xs -> Positive

    split :: Split -> xs -> SplitResult xs

    truncate :: Truncate -> xs -> TruncateResult xs

instance Positional (NonEmpty xs) where

    length = Positive.length

data Split = Split End Positive

data Truncate = Truncate TakeOrDrop End Positive

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
    Signed.Minus s -> Either.Left (Shortfall (s + 1))
    Signed.Zero -> Either.Left (Shortfall 1)
    Signed.Plus r -> Either.Right (Split (End.opposite e) r)

flipTruncate :: Positional xs => xs -> Truncate -> Either Shortfall Truncate
flipTruncate xs (Truncate tod e n) = case Positive.subtract (length xs) n of
    Signed.Minus s -> Either.Left (Shortfall s)
    Signed.Zero -> Either.Right (Truncate (TakeOrDrop.opposite tod) (End.opposite e) 0)
    Signed.Plus r -> Either.Right (Truncate (TakeOrDrop.opposite tod) (End.opposite e) r)
