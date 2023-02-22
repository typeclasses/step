module Block.Class.BiPrefix.Utilities where

import Essentials

import Data.Function (on)
import Block.Class.Positional (Take (..), Positional (..), take)
import Block.Class.ItemEquivalence.Type (ItemEquivalence, itemsEquivalent)
import Data.Ord (compare, Ordering (..))
import Block.Class.End (End (..))
import Block.Class.BiPrefix.Types (BiPrefix (..), WhichOfTwo (..))

{-| Given a pair of blocks, determine whether either is a prefix
    of the other, according to an item equivalence

If the first block is a prefix of the second, the result is
@('IsPrefix' 'First' a b)@. If the second block is a prefix of
the first, the result is @('IsPrefix' 'Second' a b)@. The fields
@(a)@ and @(b)@ constitute a partition of the larger block, where
@(a)@ is the portion equivalent to the shorter block and @(b)@
is the remainder.

If both blocks are identical (the first is a prefix of the second
and vice versa), the result is 'Same'. If neither block is a prefix
of the other, the result is 'NoPrefixRelation'. -}
biPrefix :: Positional xs =>
    ItemEquivalence xs -- ^ How to determine equivalence of two
        -- sub-blocks (such as 'Block.Class.equality')
    -> (xs, xs) -- ^ Two blocks to compare
    -> BiPrefix xs
biPrefix (itemsEquivalent -> same) pair =
    case whichIsShorter pair of
        Nothing -> if same pair then Same else NoPrefixRelation
        Just theShorter -> case take Front (length short) long of
            TakePart prefix suffix | same (prefix, short) ->
                IsPrefix theShorter prefix suffix
            _ -> NoPrefixRelation
          where
            (short, long) = case theShorter of
                First -> pair
                Second -> swap pair

  where
    swap :: (a, b) -> (b, a)
    swap (a, b) = (b, a)

    whichIsShorter :: Positional xs => (xs, xs) -> Maybe WhichOfTwo
    whichIsShorter (a, b) = case (compare `on` length) a b of
        EQ -> Nothing
        LT -> Just First
        GT -> Just Second
