module Step.Chunk where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor.Contravariant (Predicate (..), Equivalence (..))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import NatOptics.Positive.Unsafe (Positive)
import Numeric.Natural (Natural)
import Text.Show (Show)
import Data.Ord (Ord (compare), Ordering (..))
import Data.Bool (Bool (..))
import Prelude (error)

type family One (c :: Type) :: Type

class Chunk c
  where
    leftView :: c -> Pop c
    span :: Predicate (One c) -> c -> Span c
    split :: Positive Natural -> c -> Split c
    drop :: Positive Natural -> c -> Drop c
    while :: Predicate (One c) -> c -> While c
    length :: c -> Positive Natural

data StripEitherPrefix c =
    StripEitherPrefixAll
  | StripEitherPrefixFail
  | IsPrefixOf   { afterPrefix :: c }
  | IsPrefixedBy { afterPrefix :: c }

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
  deriving stock (Eq, Ord, Show)

data Split c =
    SplitInsufficient
  | Split c c
  deriving stock (Eq, Ord, Show)

data Drop c =
    DropAll
  | DropInsufficient
      { dropShortfall :: Positive Natural
      }
  | DropPart
      { dropRemainder :: c
      }
  deriving stock (Eq, Ord, Show)

data While c =
    WhileNone
  | WhilePrefix c
  | WhileAll

head :: Chunk c => c -> One c
head = popItem . leftView

stripEitherPrefix :: Chunk c => Equivalence c -> c -> c -> StripEitherPrefix c
stripEitherPrefix eq a b = case compare (length a) (length b) of
    EQ -> if getEquivalence eq a b
            then StripEitherPrefixAll
            else StripEitherPrefixFail
    LT -> case split (length a) b of
        Split a' b' -> if getEquivalence eq a a'
            then IsPrefixOf{ afterPrefix = b' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
    GT -> case split (length b) a of
        Split b' a' -> if getEquivalence eq b b'
            then IsPrefixedBy{ afterPrefix = a' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
