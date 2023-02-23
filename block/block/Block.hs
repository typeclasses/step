module Block
  (
    {- * Block types -} Seq1, ByteString1, Text1, BlockBlock (..),

    {- * Concatenation -} (++), concat, concatRefined, append,

    {- * Dealing with single items -}
            singleton,  first, last, terminal,
            pop, Pop (Pop), push, unpop, pushMaybe,

    {- * Operations involving numeric positions -}
            length, take, Take (TakeAll, TakePart, TakeInsufficient),

    {- * Single item at a numeric position -} at,

    {- * Searching for items matching a predicate -}
            span, spanPredicate, Span (SpanAll, SpanNone, SpanPart),
            find, findPredicate, Pivot (Pivot),

    {- * Prefix detection -}
            biPrefix, BiPrefix (Same, NoPrefixRelation, IsPrefix),
            WhichOfTwo (First, Second),

    {- * Item equality -}
            sameItems, sameItemsTake, sameItemsPop, sameItemsPivot,
            sameItemsSpan, foldableEqOn,

    {- * Item equivalence -}
            ItemEquivalence (..), equality,

    {- * Nullability -}
            refine, generalize, assume,

    {- * Isomorphism with NonEmpty -}
            toNonEmpty, fromNonEmpty,

    {- * End = Front | Back -} End (Front, Back),

    {- * Shortfall -} Shortfall (Shortfall),

    {- * Classes -} Singleton, Positional, Search,
            Refined, NonEmptyIso, Index, Concat, ItemEquality,

    {- * State -} State (..),
            runState, evalState, execState, stateless, get, put, modify,
  )
  where

import Block.BlockBlock
import Block.ByteString
import Block.Class
import Block.Sequence
import Block.Text
