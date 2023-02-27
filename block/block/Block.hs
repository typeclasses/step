module Block
  (
    {- * Block types -} Seq1, ByteString1, Text1, ASCII1, BlockBlock (..),

    {- * Concatenation -} (++), concat, concatRefined, append,

    {- * Dealing with single items -}
            singleton,  first, last, terminal,
            pop, Pop (Pop), push, unpop, pushMaybe,

    {- * Operations involving numeric positions -}
            length, take, Take (..),

    {- * Single item at a numeric position -} at,

    {- * Searching for items matching a predicate -}
            span, spanPredicate, Span (..),
            find, findPredicate, Pivot (..),

    {- * Prefix detection -} biPrefix, BiPrefix (..), WhichOfTwo (..),

    {- * Item equality -}
            sameItems, sameItemsTake, sameItemsPop, sameItemsPivot,
            sameItemsSpan, foldableEqOn,

    {- * Item equivalence -}
            ItemEquivalence (..), equality, itemEquality,

    {- * Nullability -}
            refine, generalize, assume,

    {- * Isomorphism with NonEmpty -}
            toNonEmpty, fromNonEmpty, foldItems,

    {- * End = Front | Back -} End (..),

    {- * Shortfall -} Shortfall (..),

    {- * Classes -} Block, Singleton, Positional, Search,
            Refined, NonEmptyIso, Index, Concat, ItemEquality, Enumerate,

    {- * State -} State (..),
            runState, evalState, execState, stateless, get, put, modify,

    {- * ASCII -} ASCII, generalizeAscii, refineAscii, assumeAscii,
  )
  where

import Block.ASCII
import Block.BlockBlock
import Block.ByteString
import Block.Class
import Block.Sequence
import Block.Text
