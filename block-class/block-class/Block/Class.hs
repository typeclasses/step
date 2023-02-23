module Block.Class
  (
    {- * Concatenation -} (++), concat, concatRefined, append,

    {- * Dealing with single items -} {- $singleton -}
            singleton,  first, last, terminal,
            pop, Pop (Pop), push, unpop, pushMaybe,

    {- * Operations involving numeric positions -} {- $positional -}
            length, take, Take (TakeAll, TakePart, TakeInsufficient),

    {- * Single item at a numeric position -} {- * index -} at,

    {- * Searching for items matching a predicate -} {- $search -}
            span, spanPredicate, Span (SpanAll, SpanNone, SpanPart),
            find, findPredicate, Pivot (Pivot),

    {- * Prefix detection -} {- $biPrefix -}
            biPrefix, BiPrefix (Same, NoPrefixRelation, IsPrefix),
            WhichOfTwo (First, Second),

    {- * Item equality -} {- $itemEquality -}
            sameItems, sameItemsTake, sameItemsPop, sameItemsPivot,
            sameItemsSpan, foldableEqOn,

    {- * Item equivalence -} {- $itemEquivalence -}
            ItemEquivalence (..), equality,

    {- * Nullability -} {- $trivializable -}
            refine, generalize, assume,

    {- * Isomorphism with NonEmpty -} {- $nonEmptyIso -}
            toNonEmpty, fromNonEmpty,

    {- * End = Front | Back -} {- $end -} End (Front, Back),

    {- * Shortfall -} {- $shortfall -} Shortfall (Shortfall),

    {- * Classes -} Singleton, Positional, Search, Refined,
            NonEmptyIso, Index, Concat, ItemEquality,

    {- * State -} State (..),
            runState, evalState, execState, stateless, get, put, modify,
  )
  where

import Block.Class.BiPrefix.Types (BiPrefix (..), WhichOfTwo (..))
import Block.Class.BiPrefix.Utilities ( biPrefix )
import Block.Class.Concat.Class (Concat (..))
import Block.Class.Concat.Utilities (append)
import Block.Class.End (End (..))
import Block.Class.Index.Class (Index (..))
import Block.Class.ItemEquality.Class (ItemEquality (..))
import Block.Class.ItemEquality.Utilities (sameItemsTake, sameItemsPop, sameItemsPivot, sameItemsSpan, foldableEqOn)
import Block.Class.ItemEquivalence.Examples (equality)
import Block.Class.ItemEquivalence.Type (ItemEquivalence (..))
import Block.Class.NonEmptyIso.Class (NonEmptyIso (..))
import Block.Class.Positional.Class (Positional (..))
import Block.Class.Positional.Types (Take(..))
import Block.Class.Refined.Class (Refined (..))
import Block.Class.Refined.Utilities (concatRefined)
import Block.Class.Search.Class (Search (..))
import Block.Class.Search.Types (Span (..), Pivot (..))
import Block.Class.Search.Utilities (findPredicate, spanPredicate)
import Block.Class.Shortfall (Shortfall (..))
import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Singleton.Types (Pop (..))
import Block.Class.Singleton.Utilities (first, last, terminal, unpop, pushMaybe)
import Block.Class.State.Types (State (..))
import Block.Class.State.Utilities (runState, evalState, execState, stateless, get, put, modify)

{- $singleton

See "Block.Class.Singleton". -}

{- $concat

To concatenate a non-empty list of blocks, use 'Data.Semigroup.sconcat'. -}

{- $positional

See "Block.Class.Positional". -}

{- $search

See "Block.Class.Search". -}

{- $biPrefix

See "Block.Class.BiPrefix". -}

{- $itemEquivalence

See "Block.Class.ItemEquivalence". -}

{- $trivializable

See "Block.Class.Trivializable" -}

{- $nonEmptyIso

See "Block.Class.NonEmptyIso". -}

{- $end

See "Block.Class.End". -}

{- $shortfall

See "Block.Class.Shortfall". -}

{- $index

See "Block.Class.Index". -}

{- $itemEquality

See "Block.Class.ItemEquality". -}
