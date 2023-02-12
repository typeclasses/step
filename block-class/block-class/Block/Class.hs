module Block.Class
  (
    {- * Block -} Block,

    {- * Item -} Item,

    {- * End -} End (Front, Back),

    {- * Singleton -} Singleton (..),

    {- * Positional -} Positional (..),
        Shortfall (Shortfall), Take (TakeAll, TakePart, TakeInsufficient),

    {- * Search -} Search (..),
        Pivot (Pivot), Span (SpanAll, SpanNone, SpanPart),

    {- * Trivializable -} Trivializable (..),

    {- * Item equivalence -} ItemEquivalence (..), equality,

    {- * Bi-prefix -} biPrefix, BiPrefix (..), WhichOfTwo (..),
  )
  where

import Block.Class.BiPrefix
import Block.Class.Block
import Block.Class.End
import Block.Class.Item
import Block.Class.ItemEquivalence
import Block.Class.Positional
import Block.Class.Search
import Block.Class.Singleton
import Block.Class.Trivializable
