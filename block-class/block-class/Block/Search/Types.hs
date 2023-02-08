module Block.Search.Types where

import Essentials

data Division found xs =
    NoDivision | Division (Maybe xs, found, Maybe xs)
    deriving stock (Eq, Ord, Show, Functor)

data Span xs =
    SpanAll
  | SpanNone
  | Span (xs, xs)
  deriving stock (Eq, Ord, Show, Functor)

data While xs =
    WhileNone
  | WhilePrefix xs
  | WhileAll
  deriving stock (Eq, Ord, Show, Functor)
