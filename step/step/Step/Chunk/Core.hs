module Step.Chunk.Core where

import Data.Eq (Eq)
import Data.Functor.Contravariant (Predicate (..))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import NatOptics.Positive.Unsafe (Positive)
import Numeric.Natural (Natural)
import Optics (Iso')
import Text.Show (Show)

type family OneOf (c :: Type) :: Type

class Chunk c
  where
    leftView :: Iso' c (Pop c)
    span :: Predicate (OneOf c) -> c -> Span c
    split :: Positive Natural -> c -> Split c
    drop :: Positive Natural -> c -> Drop c
    while :: Predicate (OneOf c) -> c -> While c
    length :: c -> Positive Natural

data Pop c =
  Pop
    { popItem :: OneOf c
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
