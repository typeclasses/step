module Step.Chunk.ListLike
  (
    NonEmptyListLike (..),
    {- * Conversion -} assume, refine,
    {- * Fold -} fold,
    {- * Testing -} genChunks,
  )
  where

import Step.Chunk

import Control.Applicative (pure, (<*>))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable)
import Data.Function (($), (&), (.), on)
import Data.Functor ((<&>))
import Data.Functor.Contravariant (Predicate (..))
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

import qualified Data.Foldable as Foldable
import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

data NonEmptyListLike c =
  NonEmptyListLike
    { nonEmptyListLike :: !c
    , nonEmptyListLikeLength :: !(Positive Natural)
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

refine :: ListLike c (Item c) => c -> Maybe (NonEmptyListLike c)
refine c = preview Positive.natPrism (fromIntegral (LL.length c)) <&> \l -> NonEmptyListLike c l

fold :: Foldable t => ListLike c (Item c) => t (NonEmptyListLike c) -> c
fold = Foldable.foldMap nonEmptyListLike

instance Eq c => Eq (NonEmptyListLike c) where
    (==) = (==) `on` nonEmptyListLike

instance Ord c => Ord (NonEmptyListLike c) where
    compare = compare `on` nonEmptyListLike

instance Show c => Show (NonEmptyListLike c) where
    showsPrec p = showsPrec p . nonEmptyListLike

type instance One (NonEmptyListLike c) = Item c

instance (Eq c, Eq (Item c), ListLike c (Item c)) => Chunk (NonEmptyListLike c)
  where

    length = nonEmptyListLikeLength

    span = \f whole -> tupleSpan (LL.span (getPredicate f) (nonEmptyListLike whole))
      where
        tupleSpan (a, b) =
            if LL.null b then SpanAll else
            if LL.null a then SpanNone else
            SpanPart (assume a) (assume b)

    drop = \n whole -> case Positive.minus (nonEmptyListLikeLength whole) n of
        Signed.Zero ->
            DropAll
        Signed.Plus _ ->
            DropPart
              { dropRemainder = assume $
                  LL.drop (fromIntegral (review Positive.refine n)) (nonEmptyListLike whole)
              }
        Signed.Minus dropShortfall ->
            DropInsufficient{ dropShortfall }

    while = \f x -> case refine (LL.takeWhile (getPredicate f) (nonEmptyListLike x)) of
        Nothing -> WhileNone
        Just y ->
            if nonEmptyListLikeLength y == nonEmptyListLikeLength x
            then WhileAll
            else WhilePrefix y

    split = \n whole -> case Positive.minus (nonEmptyListLikeLength whole) n of
        Signed.Plus _ -> Split (assume a) (assume b)
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

    stripEitherPrefix a b =
        case compare (length a) (length b) of
            EQ -> case nonEmptyListLike a == nonEmptyListLike b of
                False    ->  StripEitherPrefixFail
                True     ->  StripEitherPrefixAll
            LT -> case LL.stripPrefix (nonEmptyListLike a) (nonEmptyListLike b) of
                Nothing  ->  StripEitherPrefixFail
                Just x   ->  IsPrefixOf{ afterPrefix = assume x }
            GT -> case LL.stripPrefix (nonEmptyListLike b) (nonEmptyListLike a) of
                Nothing  ->  StripEitherPrefixFail
                Just x   ->  IsPrefixedBy{ afterPrefix = assume x }

{-| Break up a text into a list of texts

    This can be useful for generating parser inputs for testing
-}
genChunks :: (Eq xs, Eq x, ListLike xs x) => xs -> Gen [NonEmptyListLike xs]
genChunks x = case refine x of
    Nothing -> pure []
    Just y -> genChunks' y <&> LL.toList

genChunks' :: (Eq xs, Eq x, ListLike xs x) => NonEmptyListLike xs -> Gen (Seq (NonEmptyListLike xs))
genChunks' x = Gen.recursive Gen.choice [pure (x :<| Empty)] [genChunks'' x]

genChunks'' :: (Eq xs, Eq x, ListLike xs x) => NonEmptyListLike xs -> Gen (Seq (NonEmptyListLike xs))
genChunks'' x = case Positive.minus (length x) one of
    Signed.Zero -> pure (x :<| Empty)
    Signed.Plus len -> do
      Just i <- Gen.integral (Range.constant 1 (review Positive.refine len))
                  <&> preview Positive.refine
      case split i x of
          SplitInsufficient -> error "genChunks: SplitInsufficient"
          Split a b -> pure (<>) <*> genChunks' a <*> genChunks' b
    Signed.Minus _ -> error "Step.Chunk.ListLike: minus one cannot be negative"

one :: Positive Natural
one = PositiveUnsafe 1
