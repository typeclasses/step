module Block.Class.Block where

import Essentials

import Data.Functor.Contravariant (Predicate (..))
import Integer (Positive)
import Data.List.NonEmpty (NonEmpty, nonEmpty)

import qualified Data.Semigroup as Semigroup

type family Item (c :: Type) :: Type

class Semigroup c => Block c where

    singleton :: Item c -> c

    leftView :: c -> Pop c

    leftReview :: Pop c -> c

    span :: Predicate (Item c) -> c -> Span c

    divide :: (Item c -> Maybe x) -> c -> Division x c

    split :: Positive -> c -> Split c

    take :: Positive -> c -> Take c

    drop :: Positive -> c -> Drop c

    while :: Predicate (Item c) -> c -> While c

    length :: c -> Positive

    concat :: NonEmpty c -> c
    concat = Semigroup.sconcat

data Division x c =
  NoDivision | Division (Maybe c) x (Maybe c)
  deriving stock (Eq, Ord, Show, Functor)

data Pop c =
  Pop
    { popItem :: Item c
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

head :: Block c => c -> Item c
head = popItem . leftView
