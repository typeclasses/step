{-| A /block/ is a non-empty sequence. The 'Item' type family specifies the
type of the items in the sequence. Some examples of blocks may be found in the
<https://hackage.haskell.org/package/block-types block-types> package; for
example, the @Text1@ type defined there is a non-empty @Text@ value whose
@Item@ type is @Char@.

The @block-class@ package, aiming to be minimal in its dependencies, provides
instances for only one type: 'NonEmpty'. The @Item@ type of @(NonEmpty x)@ is
@(x)@. The NonEmpty instances are intended primarily for demonstration purposes.
Some of the NonEmpty operations are asymptotically poor, particularly those
that deal with the list from its back end. -}
module Block.Class
  (
    {- * Dealing with single items -} {- $singleton -}
        singleton, pop, push, unpop, head,

    {- * Operations involving numeric positions -} length, take,
        Take (TakeAll, TakePart, TakeInsufficient),

    {- * Searching for items matching a predicate -}
        span, Span (SpanAll, SpanNone, SpanPart),
        find, Pivot (Pivot),

    {- * Prefix detection -}
        biPrefix, BiPrefix (Same, NoPrefixRelation, IsPrefix),
        WhichOfTwo (First, Second), ItemEquivalence (..), equality,

    {- * Nullability -} refine, generalize, assume,

    {- * End = Front | Back -} End (Front, Back),

    {- * Shortfall -} Shortfall (Shortfall),

    {- * Families -} Item, Nullable,

    {- * Classes -} Block, Singleton, Positional, Search, Trivializable,

    {- * NonEmpty -} {- $nonEmpty -} NonEmpty ((:|)),
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
import Block.Class.Positional.Types (Take(..))
import Block.Class.Search.Class (Search (..))
import Block.Class.Search.Types (Span (..), Pivot (..))
import Block.Class.Shortfall (Shortfall (..))
import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Singleton.Utilities (head, unpop)
import Block.Class.Trivializable.Class (Trivializable (..), Nullable)

import Data.List.NonEmpty (NonEmpty (..))

{- $singleton

See "Block.Class.Singleton". -}

{- $nonEmpty

See "Data.List.NonEmpty". -}
