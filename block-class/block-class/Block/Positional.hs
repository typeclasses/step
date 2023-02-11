module Block.Positional
  (
    {- * Class -} Positional (..),
    {- * Types -} Shortfall (..), Split (..), Drop (..), Take (..),
    {- * Utilities -} flipSplitAmount,
  )
  where

import Essentials

import Block.Singleton.Class

import Integer (Positive)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Block.End (End (..))
import Data.Either (Either)
import Prelude ((+), (-), error)

import qualified Integer.Positive as Positive
import qualified Data.Either as Either
import qualified Integer.Signed as Signed
import qualified Integer.Natural as Natural
import qualified Data.List.NonEmpty as NonEmpty
import qualified Integer

class (Singleton xs) => Positional xs where

    length :: xs -> Positive

    split :: End -> Positive -> xs -> Split xs

    take :: End -> Positive -> xs -> Take xs

    drop :: End -> Positive -> xs -> Drop xs

instance Positional (NonEmpty xs) where

    length :: NonEmpty xs -> Positive
    length = Positive.length

    split :: End -> Positive -> NonEmpty xs -> Split (NonEmpty xs)
    split = \e n xs -> case e of
        Front -> splitFront n xs
        Back -> case flipSplitAmount xs n of
            Either.Left s -> SplitInsufficient s
            Either.Right n' -> splitFront n' xs
      where
        splitFront n xs =
            let (a, b) = NonEmpty.splitAt (Integer.yolo n) xs
            in case (nonEmpty a, nonEmpty b) of
                (Nothing, _) -> error "First part of NonEmpty.splitAt \
                                \should be non-empty, given a positive index"
                (_, Nothing) -> SplitInsufficient (Shortfall (n + 1 - length xs))
                (Just a', Just b') -> Split a' b'

    take :: End -> Positive -> NonEmpty xs -> Take (NonEmpty xs)
    take = _

    drop :: End -> Positive -> NonEmpty xs -> Drop (NonEmpty xs)
    drop = _

{-| (Shortfall /n/) indicates that an operation which failed
    would require a block operand to have /n/ more items. -}
newtype Shortfall = Shortfall Positive
    deriving stock (Eq, Ord, Show)

data Split xs =
    SplitInsufficient Shortfall
  | Split xs xs
  deriving stock (Eq, Ord, Show, Functor)

data Drop xs =
    DropAll
  | DropInsufficient Shortfall
  | DropPart xs
  deriving stock (Eq, Ord, Show, Functor)

data Take xs =
    TakeAll
  | TakeInsufficient Shortfall
  | TakePart xs
  deriving stock (Eq, Ord, Show, Functor)

{-| Produces an amount that is equivalent to the original amount
    when used with 'split' but is expressed in the opposite direction

This operation fails if the split would fail. -}
flipSplitAmount :: Positional xs => xs -> Positive -> Either Shortfall Positive
flipSplitAmount xs n = case Positive.subtract (length xs) n of
    Signed.Plus r -> Either.Right r
    Signed.NotPlus s -> Either.Left $ Shortfall $ Natural.addOne s
