module Block.ListLike.Type
  (
    {- * Type -} LL1,
  )
  where

import Essentials
import Block.Class

import Data.Function (on)
import Data.Functor.Contravariant (Predicate (..))
import Data.ListLike (ListLike)
import Data.Maybe (fromMaybe)
import Data.Ord (Ord (compare))
import Data.String (IsString (..))
import GHC.Exts (IsList (..), Item)
import Integer (Positive, Signed (..))
import Prelude (error, (+))
import Text.Show (Show (showsPrec))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Int (Int)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe
import qualified Data.ListLike as LL
import qualified Data.Maybe as Maybe
import qualified Integer.Positive as Positive
import qualified Integer

newtype LL1 x xs = LL1 xs
    deriving newtype (Eq, Ord, Show, Semigroup)

instance (IsString xs, ListLike xs x) => IsString (LL1 x xs) where
    fromString = fromString >>> refine >>>
        fromMaybe (error "LL1 fromString: empty")

instance (ListLike xs x) => Refined x xs (LL1 x xs) where

    refine :: xs -> Maybe (LL1 x xs)
    refine x = if LL.null x then Nothing else Just (LL1 x)

    generalize :: LL1 x xs -> xs
    generalize (LL1 x) = x

    assume :: xs -> LL1 x xs
    assume = LL1

instance (ListLike xs x) => NonEmptyIso x (LL1 x xs) where

    toNonEmpty :: LL1 x xs -> NonEmpty x
    toNonEmpty = generalize >>> LL.toList >>> nonEmpty >>> Maybe.fromJust

    fromNonEmpty :: ListLike xs x => NonEmpty x -> LL1 x xs
    fromNonEmpty = NonEmpty.toList >>> LL.fromList >>> assume

instance (ListLike xs x, Semigroup xs) => Positional x (LL1 x xs) where

    length :: LL1 x xs -> Positive
    length = generalize >>> LL.length >>> intPositive

    take :: End -> Positive -> LL1 x xs -> Take (LL1 x xs)
    take end n x = case Integer.subtract (length x) n of
        Zero -> TakeAll
        Minus s -> TakeInsufficient (Shortfall s)
        Plus n' -> x & generalize
            & splitAtPositive case end of { Front -> n; Back -> n' }
            & swapIfBack end & tupleTakePart

instance (ListLike xs x, Semigroup xs) => Singleton x (LL1 x xs) where

    singleton :: x -> LL1 x xs
    singleton = LL.singleton >>> assume

    push :: End -> x -> LL1 x xs -> LL1 x xs
    push end x = generalize >>> cons' end x >>> assume

    pop :: End -> LL1 x xs -> Pop x (LL1 x xs)
    pop end = generalize >>> uncons' end >>> tuplePop

instance Search x (LL1 x xs) where

    span :: End -> (x -> Bool) -> LL1 x xs -> Span (LL1 x xs)
    span = _

    find :: End -> (x -> Maybe found) -> LL1 x xs -> Maybe (Pivot found (LL1 x xs))
    find = _

instance (ListLike xs x) => Block x (LL1 x xs) where

    -- length = length

    -- singleton x = LL1 (LL.singleton x) 1

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
    --         case refine b :: Maybe (LL1 c) of
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
    --                 Plus n -> Just (LL1 b n )
    --                 _ -> Nothing
    --         }

    -- push Left (Pop x xs) = case xs of
    --     Nothing -> singleton x
    --     Just xs' -> LL1 (LL.cons x (generalize xs')) (length xs' + 1)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

swapIfBack :: End -> (a, a) -> (a, a)
swapIfBack = \case Front -> id; Back -> swap

tupleTakePart :: ListLike xs x =>
    (xs, xs) -- ^ Assumption: Both are be non-empty
    -> Take (LL1 x xs)
tupleTakePart (a, b) = TakePart (assume a) (assume b)

positiveInt :: Positive -- ^ Assumption: Is convertible to Int
    -> Int
positiveInt = Positive.toInt >>> Maybe.fromJust

splitAtPositive :: ListLike xs x =>
    Positive -- ^ Assumption: Is convertible to Int
    -> xs -> (xs, xs)
splitAtPositive n = LL.splitAt (positiveInt n)

cons' :: ListLike xs x => End -> x -> xs -> xs
cons' end x xs = case end of Front -> LL.cons x xs; Back -> LL.snoc xs x

uncons' :: ListLike xs x => End -> xs -> (x, xs)
uncons' end xs = case end of
    Front -> (LL.head xs, LL.tail xs)
    Back  -> (LL.last xs, LL.init xs)

tuplePop :: Refined x nul xs => (x, nul) -> Pop x xs
tuplePop (a, b) = Pop a (refine b)

intPositive :: Int -> Positive
intPositive = Positive.fromInt >>> Maybe.fromJust
