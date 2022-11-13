module Chunk where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (Functor, fmap)
import Data.Functor.Contravariant (Predicate (..))
import Data.Kind (Type)
import Data.Maybe (Maybe (..), maybe)
import Data.Ord (Ord)
import Integer (Positive)
import Text.Show (Show)
import Data.Ord (Ord (compare), Ordering (..))
import Data.Bool (Bool (..))
import Prelude (error)
import Data.Semigroup (Semigroup)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Monoid (Monoid)

import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup

type family One (c :: Type) :: Type

class Semigroup c => Chunk c
  where
    leftView :: c -> Pop c

    span :: Predicate (One c) -> c -> Span c

    split :: Positive -> c -> Split c

    take :: Positive -> c -> Take c

    drop :: Positive -> c -> Drop c

    while :: Predicate (One c) -> c -> While c

    length :: c -> Positive

    concat :: NonEmpty c -> c
    concat = Semigroup.sconcat

type family Nullable (c :: Type) :: Type

class (Chunk c, Monoid (Nullable c)) => Trivializable c
  where
    refine :: Nullable c -> Maybe c

    generalize :: c -> Nullable c

data StripEitherPrefix c =
    StripEitherPrefixAll
  | StripEitherPrefixFail
  | IsPrefixOf   { commonPart :: c, extraPart :: c }
  | IsPrefixedBy { commonPart :: c, extraPart :: c }

data Pop c =
  Pop
    { popItem :: One c
    , popRemainder :: Maybe c
    }

data Span c =
    SpanAll
  | SpanNone
  | SpanPart
      { spannedPart :: c
      , spanRemainder :: c
      }
  deriving (Eq, Ord, Show, Functor)

data Split c =
    SplitInsufficient
  | Split c c
  deriving (Eq, Ord, Show, Functor)

data Drop c =
    DropAll
  | DropInsufficient
      { dropShortfall :: Positive
      }
  | DropPart
      { dropRemainder :: c
      }
  deriving (Eq, Ord, Show, Functor)

data Take c =
    TakeAll
  | TakeInsufficient
      { takeShortfall :: Positive
      }
  | TakePart
      { takePart :: c
      }
  deriving (Eq, Ord, Show, Functor)

data While c =
    WhileNone
  | WhilePrefix c
  | WhileAll
  deriving (Eq, Ord, Show, Functor)

concatMaybe :: Chunk c => [c] -> Maybe c
concatMaybe = fmap concat . nonEmpty

concatTrivialize :: Trivializable c => [c] -> Nullable c
concatTrivialize = maybe Monoid.mempty generalize . concatMaybe

head :: Chunk c => c -> One c
head = popItem . leftView

-- | An equivalence on characters, expressed as an equivalence on chunks.
--
-- It must be the case that chunks /a/ and /b/ are equivalent iff the length
-- of /a/ and /b/ is /l/ and /a[i] ~ b[i]/ for all /i = [1 .. l]/ according
-- to the character equivalence.
--
newtype ChunkCharacterEquivalence c =
    ChunkCharacterEquivalence{ chunksEquivalent :: c -> c -> Bool }

stripEitherPrefix :: Chunk c => ChunkCharacterEquivalence c -> c -> c -> StripEitherPrefix c
stripEitherPrefix eq a b = case compare (length a) (length b) of
    EQ -> if chunksEquivalent eq a b
            then StripEitherPrefixAll
            else StripEitherPrefixFail
    LT -> case split (length a) b of
        Split a' b' -> if chunksEquivalent eq a a'
            then IsPrefixOf{ commonPart = a', extraPart = b' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
    GT -> case split (length b) a of
        Split b' a' -> if chunksEquivalent eq b b'
            then IsPrefixedBy{ commonPart = b', extraPart = a' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
