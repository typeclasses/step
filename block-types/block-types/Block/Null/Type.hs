module Block.Null.Type
  (
    {- * Type -} NotNull,
  )
  where

import Essentials
import Block.Class

import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Integer (Positive, Signed (..))
import Prelude (error)
import Data.List.NonEmpty (NonEmpty)
import Block.Null.Class (Null)

import qualified Data.Maybe as Maybe
import qualified Integer.Positive as Positive
import qualified Integer
import qualified Block.Null.Class as Null

newtype NotNull x xs = NotNull xs
    deriving newtype (Eq, Ord, Show, Semigroup)

instance (IsString xs, Null x xs) => IsString (NotNull x xs) where
    fromString = fromString >>> refine >>>
        fromMaybe (error "NotNull fromString: empty")

instance (Null x xs) => Refined x xs (NotNull x xs) where

    refine :: xs -> Maybe (NotNull x xs)
    refine = Null.notNullMaybe >>> fmap NotNull

    generalize :: NotNull x xs -> xs
    generalize (NotNull x) = x

    assume :: xs -> NotNull x xs
    assume = NotNull

instance (Null x xs) => NonEmptyIso x (NotNull x xs) where

    toNonEmpty :: End -> NotNull x xs -> NonEmpty x
    toNonEmpty end = generalize >>> Null.toNonEmpty end >>> Maybe.fromJust

    fromNonEmpty :: Null x xs => End -> NonEmpty x -> NotNull x xs
    fromNonEmpty end = Null.fromNonEmpty end >>> assume

instance (Null x xs) => Positional x (NotNull x xs) where

    length :: NotNull x xs -> Positive
    length = generalize >>> Null.length >>> Positive.fromNatural >>> Maybe.fromJust

    take :: End -> Positive -> NotNull x xs -> Take (NotNull x xs)
    take end n x = case Integer.subtract (length x) n of
        Zero -> TakeAll
        Minus s -> TakeInsufficient (Shortfall s)
        Plus n' -> x & generalize
            & Null.splitAt (Positive.toNatural case end of { Front -> n; Back -> n' })
            & case end of { Front -> id; Back -> \(a, b) -> (b, a) }
            & \(a, b) -> TakePart (assume a) (assume b)

instance (Null x xs) => Singleton x (NotNull x xs) where

    singleton :: x -> NotNull x xs
    singleton = Null.singleton >>> assume

    push :: End -> x -> NotNull x xs -> NotNull x xs
    push end x = generalize >>> Null.push end x >>> assume

    pop :: End -> NotNull x xs -> Pop x (NotNull x xs)
    pop end = generalize >>> Null.pop end >>> Maybe.fromJust
        >>> (\(a, b) -> Pop a (refine b))

instance (Null x xs) => Search x (NotNull x xs) where

    span :: End -> (x -> Bool) -> NotNull x xs -> Span (NotNull x xs)
    span end p = generalize >>> Null.span end p
        >>> (\(a, b) -> (refine a, refine b))
        >>> \case
            (Nothing, _) -> SpanNone
            (Just _, Nothing) -> SpanAll
            (Just x, Just y) -> SpanPart x y

    find :: End -> (x -> Maybe found) -> NotNull x xs -> Maybe (Pivot found (NotNull x xs))
    find end f = generalize >>> Null.find end f
        >>> fmap (\(a, x, b) -> Pivot (refine a) x (refine b))
