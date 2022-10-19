module Step.Chunk.ListLike.Core (NonEmptyListLike (..), assume) where

import Step.Chunk hiding (length, generalize)
import qualified Step.Chunk as Chunk

import Control.Applicative (pure, (<*>))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable)
import Data.Function (($), (&), (.), on)
import Data.Functor ((<&>), fmap)
import Data.Functor.Contravariant (Predicate (..), Equivalence (..))
import Data.ListLike (ListLike)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Ord (Ord (compare), Ordering (..))
import Data.Sequence (Seq (..))
import Data.String (IsString (..))
import GHC.Exts (IsList (..))
import Hedgehog (Gen)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import Numeric.Natural (Natural)
import Optics (preview, review)
import Prelude (error)
import Prelude (fromIntegral)
import Text.Show (Show (showsPrec))
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid)

import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as Foldable
import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

data NonEmptyListLike c =
  NonEmptyListLike
    { generalize :: !c
    , length :: !(Positive Natural)
    }

instance (IsString c, ListLike c (Item c)) => IsString (NonEmptyListLike c)
  where
    fromString x =
        fromMaybe (error "NonEmptyListLike fromString: empty") $
            refine (fromString x)

instance Semigroup c => Semigroup (NonEmptyListLike c)
  where
    NonEmptyListLike c1 l1 <> NonEmptyListLike c2 l2 =
        NonEmptyListLike (c1 <> c2) (Positive.plus l1 l2)

assume :: ListLike c (Item c) => c -> NonEmptyListLike c
assume c = NonEmptyListLike c (PositiveUnsafe (fromIntegral (LL.length c)))

type instance Nullable (NonEmptyListLike c) = c

instance (Monoid c, ListLike c (Item c)) => Trivializable (NonEmptyListLike c)
  where
    refine c = preview Positive.natPrism (fromIntegral (LL.length c)) <&> \l -> NonEmptyListLike c l
    generalize = generalize

instance Eq c => Eq (NonEmptyListLike c) where
    (==) = (==) `on` generalize

instance Ord c => Ord (NonEmptyListLike c) where
    compare = compare `on` generalize

instance Show c => Show (NonEmptyListLike c) where
    showsPrec p = showsPrec p . generalize

type instance One (NonEmptyListLike c) = Item c

instance (ListLike c (Item c)) => Chunk (NonEmptyListLike c)
  where

    length = length

    span = \f whole -> tupleSpan (LL.span (getPredicate f) (generalize whole))
      where
        tupleSpan (a, b) =
            if LL.null b then SpanAll else
            if LL.null a then SpanNone else
            SpanPart (assume a) (assume b)

    drop = \n whole -> case Positive.minus (length whole) n of
        Signed.Zero ->
            DropAll
        Signed.Plus _ ->
            DropPart
              { dropRemainder = assume $
                  LL.drop (fromIntegral (review Positive.refine n)) (generalize whole)
              }
        Signed.Minus dropShortfall ->
            DropInsufficient{ dropShortfall }

    take = \n whole -> case Positive.minus (length whole) n of
        Signed.Zero ->
            TakeAll
        Signed.Plus{} ->
            TakePart{ takePart = assume (LL.take (fromIntegral (review Positive.refine n)) (generalize whole)) }
        Signed.Minus takeShortfall ->
            TakeInsufficient{ takeShortfall }

    while = \f x -> case refine (LL.takeWhile (getPredicate f) (generalize x)) of
        Nothing -> WhileNone
        Just y ->
            if length y == length x
            then WhileAll
            else WhilePrefix y

    split = \n whole -> case Positive.minus (length whole) n of
        Signed.Plus _ -> Split (assume a) (assume b)
          where
            (a, b) = LL.splitAt
                (fromIntegral (review Positive.refine n))
                (generalize whole)
        _ -> SplitInsufficient

    leftView a = a
        & generalize
        & LL.uncons
        & fromMaybe (error "ListLike leftViewIso")
        & \(x, b) -> Pop
            { popItem = x
            , popRemainder =
                case Positive.minus (length a) (PositiveUnsafe 1) of
                    Signed.Plus n -> Just (NonEmptyListLike b n )
                    _ -> Nothing
            }
