module Step.Chunk.ListLike where

import Step.Chunk.Core

import Data.Eq (Eq ((==)))
import Data.Function (($), (&), (.), on)
import Data.Functor ((<&>))
import Data.Functor.Contravariant (Predicate (..))
import Data.ListLike (ListLike)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Ord (Ord (compare))
import GHC.Exts (IsList (..))
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import Numeric.Natural (Natural)
import Optics (preview, review)
import Prelude (error)
import Prelude (fromIntegral)
import Text.Show (Show (showsPrec))

import qualified Data.ListLike as LL
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

data NonEmptyListLike c =
  NonEmptyListLike
    { nonEmptyListLike :: !c
    , nonEmptyListLikeLength :: !(Positive Natural)
    }

assumeNonEmptyListLike :: ListLike c (Item c) => c -> NonEmptyListLike c
assumeNonEmptyListLike c = NonEmptyListLike c (PositiveUnsafe (fromIntegral (LL.length c)))

maybeNonEmptyListLike :: ListLike c (Item c) => c -> Maybe (NonEmptyListLike c)
maybeNonEmptyListLike c = preview Positive.natPrism (fromIntegral (LL.length c)) <&> \l -> NonEmptyListLike c l

instance Eq c => Eq (NonEmptyListLike c) where
    (==) = (==) `on` nonEmptyListLike

instance Ord c => Ord (NonEmptyListLike c) where
    compare = compare `on` nonEmptyListLike

instance Show c => Show (NonEmptyListLike c) where
    showsPrec p = showsPrec p . nonEmptyListLike

type instance OneOf (NonEmptyListLike c) = Item c

instance ListLike c (Item c) => Chunk (NonEmptyListLike c)
  where

    length = nonEmptyListLikeLength

    span = \f whole ->
        tupleSpan (LL.span (getPredicate f) (nonEmptyListLike whole))
      where
        tupleSpan (a, b) =
            if LL.null b then SpanAll else
            if LL.null a then SpanNone else
            SpanPart (assumeNonEmptyListLike a) (assumeNonEmptyListLike b)

    drop = \n whole ->
        case Positive.minus (nonEmptyListLikeLength whole) n of
            Signed.Zero ->
                DropAll
            Signed.Plus _ ->
                DropPart
                  { dropRemainder = assumeNonEmptyListLike $
                      LL.drop (fromIntegral (review Positive.refine n)) (nonEmptyListLike whole)
                  }
            Signed.Minus dropShortfall ->
                DropInsufficient{ dropShortfall }

    while = \f x ->
        case maybeNonEmptyListLike
              (LL.takeWhile (getPredicate f) (nonEmptyListLike x))
          of
            Nothing -> WhileNone
            Just y ->
                if nonEmptyListLikeLength y == nonEmptyListLikeLength x
                then WhileAll
                else WhilePrefix y

    split = \n whole ->
        case Positive.minus (nonEmptyListLikeLength whole) n of
            Signed.Plus _ -> Split (assumeNonEmptyListLike a) (assumeNonEmptyListLike b)
              where
                (a, b) = LL.splitAt
                    (fromIntegral (review Positive.refine n))
                    (nonEmptyListLike whole)
            _ -> SplitInsufficient

    leftView a = a
        & nonEmptyListLike
        & LL.uncons
        & fromMaybe (error "ListLike leftViewIso")
        & \(x, b) -> Pop
            { popItem = x
            , popRemainder =
                case Positive.minus (nonEmptyListLikeLength a) (PositiveUnsafe 1) of
                    Signed.Plus n -> Just (NonEmptyListLike b n )
                    _ -> Nothing
            }
