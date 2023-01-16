module Block.Class where

import Essentials

import Data.Functor.Contravariant (Predicate (..))
import Integer (Positive)
import Data.Ord (Ord (compare), Ordering (..))
import Prelude (error)
import Data.List.NonEmpty (NonEmpty, nonEmpty)

import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup

type family One (c :: Type) :: Type

class Semigroup c => Block c
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

class (Block c, Monoid (Nullable c)) => Trivializable c
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
  deriving stock (Eq, Ord, Show, Functor)

data Split c =
    SplitInsufficient
  | Split c c
  deriving stock (Eq, Ord, Show, Functor)

data Drop c =
    DropAll
  | DropInsufficient
      { dropShortfall :: Positive
      }
  | DropPart
      { dropRemainder :: c
      }
  deriving stock (Eq, Ord, Show, Functor)

data Take c =
    TakeAll
  | TakeInsufficient
      { takeShortfall :: Positive
      }
  | TakePart
      { takePart :: c
      }
  deriving stock (Eq, Ord, Show, Functor)

data While c =
    WhileNone
  | WhilePrefix c
  | WhileAll
  deriving stock (Eq, Ord, Show, Functor)

concatMaybe :: Block c => [c] -> Maybe c
concatMaybe = fmap concat . nonEmpty

concatTrivialize :: Trivializable c => [c] -> Nullable c
concatTrivialize = maybe Monoid.mempty generalize . concatMaybe

head :: Block c => c -> One c
head = popItem . leftView

-- | An equivalence on characters, expressed as an equivalence on blocks.
--
-- It must be the case that blocks /a/ and /b/ are equivalent iff the length
-- of /a/ and /b/ is /l/ and /a[i] ~ b[i]/ for all /i = [1 .. l]/ according
-- to the character equivalence.
--
newtype BlockCharacterEquivalence c =
    BlockCharacterEquivalence{ blocksEquivalent :: c -> c -> Bool }

stripEitherPrefix :: Block c => BlockCharacterEquivalence c -> c -> c -> StripEitherPrefix c
stripEitherPrefix eq a b = case compare (length a) (length b) of
    EQ -> if blocksEquivalent eq a b
            then StripEitherPrefixAll
            else StripEitherPrefixFail
    LT -> case split (length a) b of
        Split a' b' -> if blocksEquivalent eq a a'
            then IsPrefixOf{ commonPart = a', extraPart = b' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
    GT -> case split (length b) a of
        Split b' a' -> if blocksEquivalent eq b b'
            then IsPrefixedBy{ commonPart = b', extraPart = a' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
