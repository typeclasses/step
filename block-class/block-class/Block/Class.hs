module Block.Class
  (
    {- * Concatenation -} (++), concat, concatRefined, append,

    {- * Dealing with single items -} {- $singleton -}
            singleton,  first, last, terminal,
            pop, Pop (..), push, unpop, pushMaybe,

    {- * Operations involving numeric positions -} {- $positional -}
            length, take, Take (..),

    {- * Single item at a numeric position -} {- $index -} at,

    {- * Searching for items matching a predicate -} {- $search -}
            span, spanPredicate, Span (..),
            find, findPredicate, Pivot (..),

    {- * Prefix detection -} {- $biPrefix -}
            biPrefix, BiPrefix (..),
            WhichOfTwo (First, Second),

    {- * Item equality -} {- $itemEquality -}
            sameItems, sameItemsTake, sameItemsPop, sameItemsPivot,
            sameItemsSpan, foldableEqOn,

    {- * Item equivalence -} {- $itemEquivalence -}
            ItemEquivalence (..), equality, itemEquality,

    {- * Nullability -} {- $trivializable -}
            refine, generalize, assume,

    {- * Isomorphism with NonEmpty -} {- $nonEmptyIso -}
            toNonEmpty, fromNonEmpty, foldItems,

    {- * End = Front | Back -} {- $end -} End (..),

    {- * Shortfall -} {- $shortfall -} Shortfall (..),

    {- * Classes -} {- $classes -} Block, Singleton, Positional, Search,
            Refined, NonEmptyIso, Index, Concat, ItemEquality, Enumerate,

    {- * State -} State (..), StateResult (..),
            runState, evalState, execState, stateless, get, put, modify,
  )
  where

import Block.Class.Block
import Block.Class.ItemEquality
import Block.Class.Refined

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

{- $classes

See "Block.Class.Classes", "Block.Class.ClassNames"
-}
