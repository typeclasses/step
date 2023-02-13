module Block.Class.Search.Types where

import Essentials

{-| The result of 'Block.Class.find' -}
data Pivot p xs =
    Pivot (Maybe xs) p (Maybe xs)
  deriving stock (Eq, Ord, Show, Functor)

{-| The result of 'Block.Class.span' -}
data Span xs =
    SpanPart{ spanned :: xs, remainder :: xs }
      -- ^ Some items were spanned by the predicate
  | SpanNone
      -- ^ The first item does not satisfy the predicate
  | SpanAll
      -- ^ All items satisfy the predicate
  deriving stock (Eq, Ord, Show, Functor)
