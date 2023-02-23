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
import qualified Data.Foldable as Foldable

import qualified Data.Maybe as Maybe
import qualified Integer.Positive as Positive
import qualified Integer
import qualified Block.Null.Class as Null

newtype NotNull x xs = NotNull xs
    deriving newtype (Eq, Ord, Show, Semigroup)

instance (IsString xs, Null x xs) => IsString (NotNull x xs) where
    fromString = fromString >>> refine >>>
        fromMaybe (error "NotNull fromString: empty")

instance (Null x xs) => Refined xs (NotNull x xs) where

    refine :: xs -> Maybe (NotNull x xs)
    refine = Null.notNullMaybe >>> fmap NotNull

    generalize :: NotNull x xs -> xs
    generalize (NotNull x) = x

instance Eq xs => ItemEquality (NotNull x xs) where
    sameItems = (==)

instance (Null x xs) => Concat (NotNull x xs) where

    (++) :: NotNull x xs -> NotNull x xs -> NotNull x xs
    NotNull a ++ NotNull b = NotNull $ (Null.++) a b

    concat :: End -> NonEmpty (NotNull x xs) -> NotNull x xs
    concat end = Foldable.toList >>> fmap generalize >>> Null.concat end >>> NotNull

instance (Null x xs) => NonEmptyIso x (NotNull x xs) where

    toNonEmpty :: End -> NotNull x xs -> NonEmpty x
    toNonEmpty end = generalize >>> Null.toNonEmpty end >>> Maybe.fromJust

    fromNonEmpty :: End -> NonEmpty x -> NotNull x xs
    fromNonEmpty end = Null.fromNonEmpty end >>> NotNull

instance (Null x xs) => Positional (NotNull x xs) where

    length :: NotNull x xs -> Positive
    length = generalize >>> Null.length >>> Positive.fromNatural >>> Maybe.fromJust

    take :: End -> Positive -> NotNull x xs -> Take (NotNull x xs)
    take end n x = case Integer.subtract (length x) n of
        Zero -> TakeAll
        Minus s -> TakeInsufficient (Shortfall s)
        Plus n' -> x & generalize
            & Null.splitAt (Positive.toNatural case end of { Front -> n; Back -> n' })
            & case end of { Front -> id; Back -> \(a, b) -> (b, a) }
            & \(a, b) -> TakePart (NotNull a) (NotNull b)

instance (Null x xs) => Index x (NotNull x xs) where

    at :: End -> Positive -> NotNull x xs -> Maybe x
    at end n (NotNull xs) = Null.at end n xs

instance (Null x xs) => Singleton x (NotNull x xs) where

    singleton :: x -> NotNull x xs
    singleton = Null.singleton >>> NotNull

    push :: End -> x -> NotNull x xs -> NotNull x xs
    push end x = generalize >>> Null.push end x >>> NotNull

    pop :: End -> NotNull x xs -> Pop x (NotNull x xs)
    pop end = generalize >>> Null.pop end >>> Maybe.fromJust
        >>> (\(a, b) -> Pop a (refine b))

instance (Null x xs) => Search x (NotNull x xs) where

    span :: Monad m => End -> (x -> m Bool) -> NotNull x xs
        -> m (Span (NotNull x xs))
    span end f xs =
        Null.span end f (generalize xs)
        <&> (\(a, b) -> (refine a, refine b))
        <&> \case
            (Nothing, _) -> SpanNone
            (Just _, Nothing) -> SpanAll
            (Just x, Just y) -> SpanPart x y

    find :: Monad m => End -> (x -> m (Maybe found)) -> NotNull x xs
        -> m (Maybe (Pivot found (NotNull x xs)))
    find end f xs =
        Null.find end f (generalize xs)
        <&> fmap (\(a, x, b) -> Pivot (refine a) x (refine b))
