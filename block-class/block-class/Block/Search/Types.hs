module Block.Search.Types where

import Essentials

data Division x c =
    NoDivision | Division (Maybe c) x (Maybe c)
    deriving stock (Eq, Ord, Show, Functor)

data Span c =
    SpanAll
  | SpanNone
  | SpanPart
      { spannedPart :: c
      , spanRemainder :: c
      }
  deriving stock (Eq, Ord, Show, Functor)

data While c =
    WhileNone
  | WhilePrefix c
  | WhileAll
  deriving stock (Eq, Ord, Show, Functor)
