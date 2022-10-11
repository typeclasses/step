module Step.Chunk where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor.Contravariant (Predicate (..))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import NatOptics.Positive.Unsafe (Positive)
import Numeric.Natural (Natural)
import Text.Show (Show)

type family One (c :: Type) :: Type

class Chunk c
  where
    leftView :: c -> Pop c
    span :: Predicate (One c) -> c -> Span c
    split :: Positive Natural -> c -> Split c
    drop :: Positive Natural -> c -> Drop c
    while :: Predicate (One c) -> c -> While c
    length :: c -> Positive Natural
    stripEitherPrefix :: c -> c -> StripEitherPrefix c

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
