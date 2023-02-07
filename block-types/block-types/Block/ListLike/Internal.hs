{-# language UndecidableInstances #-}

module Block.ListLike.Internal where

import Essentials

import Block.Class hiding (length, generalize, Item)
import qualified Block.Class as Chunk
import qualified Block.Class as Block

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

import qualified Data.ListLike as LL
import qualified Data.Maybe as Maybe
import qualified Integer.Positive as Positive
import qualified Integer

data LL1 c = LL1
    { generalize :: !c
    , length :: !Positive
    }

instance (IsString c, ListLike c (Item c)) => IsString (LL1 c) where
    fromString x =
        fromMaybe (error "LL1 fromString: empty") $
            refine (fromString x)

instance Semigroup c => Semigroup (LL1 c) where
    LL1 c1 l1 <> LL1 c2 l2 =
        LL1 (c1 <> c2) (l1 + l2)

type instance Nullable (LL1 c) = c

instance (Monoid c, ListLike c (Item c)) => Trivializable (LL1 c) where
    refine c = Positive.fromInt (LL.length c) <&> \l -> LL1 c l
    generalize = generalize
    assume c = LL1 c $ Integer.yolo $ LL.length c

instance Eq c => Eq (LL1 c) where
    (==) = (==) `on` generalize

instance Ord c => Ord (LL1 c) where
    compare = compare `on` generalize

instance Show c => Show (LL1 c) where
    showsPrec p = showsPrec p . generalize

type instance Block.Item (LL1 c) = Item c

instance (ListLike c (Item c)) => Block (LL1 c) where

    length = length

    singleton x = LL1 (LL.singleton x) 1

    span s = \f whole -> tupleSpan (span' (getPredicate f) (generalize whole))
      where
        span' = case s of Left -> LL.span; Right -> LL.span
        tupleSpan (a, b) =
            if LL.null b then SpanAll else
            if LL.null a then SpanNone else
            SpanPart (assume a) (assume b)

    divide Left = \f whole ->
        let
            (a, b) = LL.span (Maybe.isNothing . f) (generalize whole)
        in
            case refine b :: Maybe (LL1 c) of
                Nothing -> NoDivision
                Just (pop Left -> Pop x b') ->
                    Division (refine a) (Maybe.fromJust (f x)) b'

    drop Left = \n whole -> case Positive.subtract (length whole) n of
        Zero ->
            DropAll
        Plus _ ->
            DropPart
              { dropRemainder = assume $
                  LL.drop (Integer.yolo n) (generalize whole)
              }
        Minus dropShortfall ->
            DropInsufficient{ dropShortfall }

    take Left = \n whole -> case Positive.subtract (length whole) n of
        Zero ->
            TakeAll
        Plus{} ->
            TakePart{ takePart = assume (LL.take (Integer.yolo n) (generalize whole)) }
        Minus takeShortfall ->
            TakeInsufficient{ takeShortfall }

    while Left = \f x -> case refine (LL.takeWhile (getPredicate f) (generalize x)) of
        Nothing -> WhileNone
        Just y ->
            if length y == length x
            then WhileAll
            else WhilePrefix y

    split Left = \n whole -> case Positive.subtract (length whole) n of
        Plus _ -> Split (assume a) (assume b)
          where
            (a, b) = LL.splitAt (Integer.yolo n) (generalize whole)
        _ -> SplitInsufficient

    pop Left a = a
        & generalize
        & LL.uncons
        & fromMaybe (error "ListLike leftViewIso")
        & \(x, b) -> Pop
            { popItem = x
            , popRemainder =
                case Positive.subtract (length a) Positive.one of
                    Plus n -> Just (LL1 b n )
                    _ -> Nothing
            }

    push Left (Pop x xs) = case xs of
        Nothing -> singleton x
        Just xs' -> LL1 (LL.cons x (generalize xs')) (length xs' + 1)
