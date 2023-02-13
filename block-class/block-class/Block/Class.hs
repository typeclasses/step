module Block.Class
  (
    {- * End = Front | Back -} End (Front, Back),

    {- * Dealing with single items -}
        singleton, pop, push, unpop, head, pushMaybes,

    {- * Operations involving numeric positions -} length, take,
        Take (TakeAll, TakePart, TakeInsufficient), Shortfall (Shortfall),

    {- * Searching for items matching a predicate -} span, find,
        Pivot (Pivot), Span (SpanAll, SpanNone, SpanPart),

    {- * Prefix detection -}
        biPrefix, BiPrefix (Same, NoPrefixRelation, IsPrefix),
        WhichOfTwo (First, Second), ItemEquivalence, equality,

    {- * Nullability -} refine, generalize, assume,

    {- * Classes and families -}
        Block, Singleton, Item, Positional, Search, Trivializable, Nullable,
  )
  where

import Block.Class.BiPrefix.Types (BiPrefix (..), WhichOfTwo (..))
import Block.Class.BiPrefix.Utilities ( biPrefix )
import Block.Class.Block.Class (Block)
import Block.Class.End (End (..))
import Block.Class.Item (Item)
import Block.Class.ItemEquivalence.Examples (equality)
import Block.Class.ItemEquivalence.Type (ItemEquivalence (..))
import Block.Class.Positional.Class (Positional (..))
import Block.Class.Positional.Types (Take(..), Shortfall (..))
import Block.Class.Search.Class (Search (..))
import Block.Class.Search.Types (Span (..), Pivot (..))
import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Singleton.Utilities (head, pushMaybes, unpop)
import Block.Class.Trivializable.Class (Trivializable (..), Nullable)
