{-# language UndecidableInstances #-}

module Chunk.ListLike.Core (NonEmptyListLike (..), assume) where

import Chunk hiding (length, generalize)
import qualified Chunk as Chunk

import Data.Eq (Eq ((==)))
import Data.Function (($), (&), (.), on)
import Data.Functor ((<&>))
import Data.Functor.Contravariant (Predicate (..))
import Data.ListLike (ListLike)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Ord (Ord (compare))
import Data.String (IsString (..))
import GHC.Exts (IsList (..))
import Integer (Positive)
import Prelude (error, (+))
import Text.Show (Show (showsPrec))
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid)
import Integer.Signed (Signed (..))

import qualified Prelude as Num (Integral (..))
import qualified Data.ListLike as LL
import qualified Integer.Positive as Positive
import qualified Integer

data NonEmptyListLike c =
  NonEmptyListLike
    { generalize :: !c
    , length :: !Positive
    }

instance (IsString c, ListLike c (Item c)) => IsString (NonEmptyListLike c)
  where
    fromString x =
        fromMaybe (error "NonEmptyListLike fromString: empty") $
            refine (fromString x)

instance Semigroup c => Semigroup (NonEmptyListLike c)
  where
    NonEmptyListLike c1 l1 <> NonEmptyListLike c2 l2 =
        NonEmptyListLike (c1 <> c2) (l1 + l2)

assume :: ListLike c (Item c) => c -> NonEmptyListLike c
assume c = NonEmptyListLike c $ Integer.yolo $ LL.length c

type instance Nullable (NonEmptyListLike c) = c

instance (Monoid c, ListLike c (Item c)) => Trivializable (NonEmptyListLike c)
  where
    refine c = Positive.fromInteger (Num.toInteger (LL.length c)) <&> \l -> NonEmptyListLike c l
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

    drop = \n whole -> case Positive.subtract (length whole) n of
        Zero ->
            DropAll
        Plus _ ->
            DropPart
              { dropRemainder = assume $
                  LL.drop (Integer.yolo n) (generalize whole)
              }
        Minus dropShortfall ->
            DropInsufficient{ dropShortfall }

    take = \n whole -> case Positive.subtract (length whole) n of
        Zero ->
            TakeAll
        Plus{} ->
            TakePart{ takePart = assume (LL.take (Integer.yolo n) (generalize whole)) }
        Minus takeShortfall ->
            TakeInsufficient{ takeShortfall }

    while = \f x -> case refine (LL.takeWhile (getPredicate f) (generalize x)) of
        Nothing -> WhileNone
        Just y ->
            if length y == length x
            then WhileAll
            else WhilePrefix y

    split = \n whole -> case Positive.subtract (length whole) n of
        Plus _ -> Split (assume a) (assume b)
          where
            (a, b) = LL.splitAt (Integer.yolo n) (generalize whole)
        _ -> SplitInsufficient

    leftView a = a
        & generalize
        & LL.uncons
        & fromMaybe (error "ListLike leftViewIso")
        & \(x, b) -> Pop
            { popItem = x
            , popRemainder =
                case Positive.subtract (length a) Positive.one of
                    Plus n -> Just (NonEmptyListLike b n )
                    _ -> Nothing
            }
