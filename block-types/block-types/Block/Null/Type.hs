module Block.Null.Type
  (
    {- * Type -} NotNull,
  )
  where

import Essentials
import Block.Class

import Data.Function (on)
import Data.Functor.Contravariant (Predicate (..))
import Data.Maybe (fromMaybe)
import Data.Ord (Ord (compare))
import Data.String (IsString (..))
import GHC.Exts (IsList (..), Item)
import Integer (Positive, Signed (..))
import Prelude (error, (+))
import Text.Show (Show (showsPrec))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Int (Int)
import Block.Null.Class (Null)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe
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

    toNonEmpty :: NotNull x xs -> NonEmpty x
    toNonEmpty = generalize >>> Null.toNonEmpty >>> Maybe.fromJust

    fromNonEmpty :: Null x xs => NonEmpty x -> NotNull x xs
    fromNonEmpty = Null.fromNonEmpty >>> assume

instance (Null x xs) => Positional x (NotNull x xs) where

    length :: NotNull x xs -> Positive
    length = generalize >>> Null.length >>> Positive.fromNatural >>> Maybe.fromJust

    take :: End -> Positive -> NotNull x xs -> Take (NotNull x xs)
    take end n x = case Integer.subtract (length x) n of
        Zero -> TakeAll
        Minus s -> TakeInsufficient (Shortfall s)
        Plus n' -> x & generalize
            & Null.splitAt (Positive.toNatural case end of { Front -> n; Back -> n' })
            & swapIfBack end & tupleTakePart

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
    find = _

instance (Null x xs) => Block x (NotNull x xs) where

    -- length = length

    -- singleton x = NotNull (LL.singleton x) 1

    -- span s = \f whole -> tupleSpan (span' (getPredicate f) (generalize whole))
    --   where
    --     span' = case s of Left -> LL.span; Right -> LL.span
    --     tupleSpan (a, b) =
    --         if LL.null b then SpanAll else
    --         if LL.null a then SpanNone else
    --         SpanPart (assume a) (assume b)

    -- divide Left = \f whole ->
    --     let
    --         (a, b) = LL.span (Maybe.isNothing . f) (generalize whole)
    --     in
    --         case refine b :: Maybe (NotNull c) of
    --             Nothing -> NoDivision
    --             Just (pop Left -> Pop x b') ->
    --                 Division (refine a) (Maybe.fromJust (f x)) b'

    -- drop Left = \n whole -> case Positive.subtract (length whole) n of
    --     Zero ->
    --         DropAll
    --     Plus _ ->
    --         DropPart
    --           { dropRemainder = assume $
    --               LL.drop (Integer.yolo n) (generalize whole)
    --           }
    --     Minus dropShortfall ->
    --         DropInsufficient{ dropShortfall }

    -- take Left = \n whole -> case Positive.subtract (length whole) n of
    --     Zero ->
    --         TakeAll
    --     Plus{} ->
    --         TakePart{ takePart = assume (LL.take (Integer.yolo n) (generalize whole)) }
    --     Minus takeShortfall ->
    --         TakeInsufficient{ takeShortfall }

    -- while Left = \f x -> case refine (LL.takeWhile (getPredicate f) (generalize x)) of
    --     Nothing -> WhileNone
    --     Just y ->
    --         if length y == length x
    --         then WhileAll
    --         else WhilePrefix y

    -- split Left = \n whole -> case Positive.subtract (length whole) n of
    --     Plus _ -> Split (assume a) (assume b)
    --       where
    --         (a, b) = LL.splitAt (Integer.yolo n) (generalize whole)
    --     _ -> SplitInsufficient

    -- pop Left a = a
    --     & generalize
    --     & LL.uncons
    --     & fromMaybe (error "ListLike leftViewIso")
    --     & \(x, b) -> Pop
    --         { popItem = x
    --         , popRemainder =
    --             case Positive.subtract (length a) Positive.one of
    --                 Plus n -> Just (NotNull b n )
    --                 _ -> Nothing
    --         }

    -- push Left (Pop x xs) = case xs of
    --     Nothing -> singleton x
    --     Just xs' -> NotNull (LL.cons x (generalize xs')) (length xs' + 1)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

swapIfBack :: End -> (a, a) -> (a, a)
swapIfBack = \case Front -> id; Back -> swap

tupleTakePart :: Null x xs =>
    (xs, xs) -- ^ Assumption: Both are be non-empty
    -> Take (NotNull x xs)
tupleTakePart (a, b) = TakePart (assume a) (assume b)

positiveInt :: Positive -- ^ Assumption: Is convertible to Int
    -> Int
positiveInt = Positive.toInt >>> Maybe.fromJust

-- splitAtPositive :: Null x xs =>
--     Positive -- ^ Assumption: Is convertible to Int
--     -> xs -> (xs, xs)
-- splitAtPositive n = Null.splitAt (positiveInt n)

-- cons' :: Null x xs => End -> x -> xs -> xs
-- cons' end x xs = case end of Front -> LL.cons x xs; Back -> LL.snoc xs x

-- uncons' :: Null x xs => End -> xs -> (x, xs)
-- uncons' end xs = case end of
--     Front -> (LL.head xs, LL.tail xs)
--     Back  -> (LL.last xs, LL.init xs)

intPositive :: Int -> Positive
intPositive = Positive.fromInt >>> Maybe.fromJust
