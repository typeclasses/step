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
import Data.List.NonEmpty (NonEmpty (..))
import Block.End (End)
import Data.Either (Either)
import Prelude ((+))

import qualified Block.End as End
import qualified Integer.Positive as Positive
import qualified Data.Either as Either
import qualified Integer.Signed as Signed

class (Singleton xs) => Positional xs where

    length :: xs -> Positive

    split :: (End, Positive) -> xs -> Split xs

    take :: (End, Positive) -> xs -> Take xs

    drop :: (End, Positive) -> xs -> Drop xs

instance Positional (NonEmpty xs) where

    length = Positive.length

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
flipSplitAmount :: Positional xs => xs -> (End, Positive) -> Either Shortfall (End, Positive)
flipSplitAmount xs (e, n) = case Positive.subtract (length xs) n of
    Signed.Zero    -> Either.Left  $ Shortfall 1
    Signed.Minus s -> Either.Left  $ Shortfall (s + 1)
    Signed.Plus r  -> Either.Right $ (End.opposite e, r)
