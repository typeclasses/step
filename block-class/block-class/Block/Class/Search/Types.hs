module Block.Class.Search.Types
  (
    {- * Types -} Pivot (..), Span (..),
  )
  where

import Essentials

{-| A successful result of 'Block.Class.find' -}
data Pivot found xs =
    Pivot
      (Maybe xs) -- ^ Items that were searched before finding a match
      found -- ^ Result from a successful item match
      (Maybe xs) -- ^ Items that were not searched
  deriving stock (Eq, Ord, Show, Functor)

{-| The result of 'Block.Class.span' -}
data Span xs =
    SpanPart
      -- ^ Some items were spanned by the predicate
      { spanned :: xs -- ^ The spanned items
      , spanRemainder :: xs -- ^ The remainder
      }
  | SpanNone
      -- ^ The first item does not satisfy the predicate
  | SpanAll
      -- ^ All items satisfy the predicate
  deriving stock (Eq, Ord, Show, Functor)
