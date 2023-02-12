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
  )
  where

import Block.Class.Block
import Block.Class.Singleton
import Block.Class.Positional
import Block.Class.Search
import Block.Class.Trivializable
import Block.Class.End
import Block.Class.Item
