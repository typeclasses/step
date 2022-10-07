module Step.Chunk.Unchunked where

import Step.Chunk.Core

import Data.Bool (Bool (..))
import Data.Functor.Contravariant (Predicate (..))
import Data.Maybe (Maybe (..))
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import Numeric.Natural (Natural)
import Prelude (error)

import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

newtype Unchunked a = Unchunked a

type instance OneOf (Unchunked a) = a

instance Chunk (Unchunked a)
  where
    length _ = one

    split _ _ = SplitInsufficient

    leftView (Unchunked x) = Pop x Nothing

    span (Predicate f) (Unchunked x) = case f x of
        True -> SpanAll
        False -> SpanNone

    while (Predicate f) (Unchunked x) = case f x of
        True -> WhileAll
        False -> WhileNone

    drop n _ = case (Positive.minus n one) of
        Signed.Zero -> DropAll
        Signed.Plus n' -> DropInsufficient n'
        Signed.Minus _ -> error "positive minus one cannot be negative"

one :: Positive Natural
one = PositiveUnsafe 1
