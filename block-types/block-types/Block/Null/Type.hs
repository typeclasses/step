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
import GHC.Exts (IsList)
import Fold.ShortcutNonempty (ShortcutNonemptyFold)

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Block.Null.Class as Null
import qualified Fold.Shortcut
import qualified Integer.Positive as Positive
import qualified Integer

newtype NotNull x xs = NotNull xs
    deriving newtype (Eq, Ord, Show, Semigroup, IsList)

instance (IsString xs, Null x xs) => IsString (NotNull x xs) where
    fromString = fromString >>> refine >>>
        fromMaybe (error "NotNull fromString: empty")

instance (Eq xs, Null x xs) => Block x (NotNull x xs)

instance (Null x xs) => Refined xs (NotNull x xs) where

    refine :: xs -> Maybe (NotNull x xs)
    refine = Null.notNullMaybe >>> fmap NotNull

    generalize :: NotNull x xs -> xs
    generalize (NotNull x) = x

instance (Eq xs) => ItemEquality (NotNull x xs) where
    sameItems = (==)

instance (Null x xs) => Concat (NotNull x xs) where

    (++) :: NotNull x xs -> NotNull x xs -> NotNull x xs
    NotNull a ++ NotNull b = NotNull $ (Null.++) a b

    concat :: End -> NonEmpty (NotNull x xs) -> NotNull x xs
    concat end = Foldable.toList >>> fmap generalize >>> Null.concat end >>> NotNull

instance (Null x xs) => Enumerate x (NotNull x xs) where

    toNonEmpty :: End -> NotNull x xs -> NonEmpty x
    toNonEmpty end = generalize >>> Null.toNonEmpty end >>> Maybe.fromJust

    foldItems :: End -> ShortcutNonemptyFold x a -> NotNull x xs -> a
    foldItems end f = generalize >>>
        Null.foldItems end (Fold.Shortcut.shortcutNonemptyFold f <&> Maybe.fromJust)

instance (Null x xs) => Construct x (NotNull x xs) where

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

    find :: Monad m => End -> (x -> m (Maybe found)) -> NotNull x xs
        -> m (Maybe (Pivot found (NotNull x xs)))
    find end f xs =
        Null.find end f (generalize xs)
        <&> fmap
          ( \(Null.Pivot x (a, b) (a', b')) ->
                Pivot x
                    (assume a, refine b)
                    (refine a', assume b')
          )
