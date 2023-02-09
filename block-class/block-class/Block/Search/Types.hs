module Block.Search.Types where

import Essentials

data Pivot p xs =
    Pivot (Maybe xs) p (Maybe xs)
  deriving stock (Eq, Ord, Show, Functor)

data Span xs =
    SpanAll
  | SpanNone
  | Span xs xs
  deriving stock (Eq, Ord, Show, Functor)
