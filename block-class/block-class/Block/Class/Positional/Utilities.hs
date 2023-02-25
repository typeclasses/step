module Block.Class.Positional.Utilities
  (
    {- * Utilities -} biPrefix, sameItemsTake,
  )
  where

import Essentials

import Block.Class.End (End)
import Block.Class.ItemEquality.Class (ItemEquality (..))
import Block.Class.ItemEquivalence.Types (ItemEquivalence (equivalentItems))
import Block.Class.Positional.Class (Positional (..))
import Block.Class.Positional.Types (BiPrefix (..), WhichOfTwo (..), Take (..))
import Data.Bool ((&&))
import Data.Function (on)
import Data.Ord (Ordering (..), compare)

{-| Given a pair of blocks, determine whether either is a prefix
    of the other, according to an item equivalence

If the first block is a prefix of the second, the result is
@('IsPrefix' 'First' a b)@. If the second block is a prefix of
the first, the result is @('IsPrefix' 'Second' a b)@. The fields
@(a)@ and @(b)@ constitute a partition of the larger block, where
@(a)@ is the portion equivalent to the shorter block and @(b)@
is the remainder.

If both blocks are identical (the first is a prefix of the second
and vice versa), the result is 'BothPrefix'. If neither block is a
prefix of the other, the result is 'NoPrefixRelation'. -}
biPrefix :: Positional xs =>
    ItemEquivalence xs -- ^ How to determine equivalence of two
                       --   sub-blocks (such as 'Block.Class.equality')
    -> End
    -> (xs, xs) -- ^ Two blocks to compare
    -> BiPrefix xs
biPrefix (equivalentItems -> same) end pair =
    case whichIsShorter pair of
        Nothing -> if same pair then BothPrefix else NoPrefixRelation
        Just theShorter -> case take end (length short) long of
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

sameItemsTake :: ItemEquality xs => Take xs -> Take xs -> Bool
sameItemsTake = \case
    TakeAll -> \case TakeAll -> True; _ -> False
    TakeInsufficient s1 -> \case TakeInsufficient s2 -> s1 == s2; _ -> False
    TakePart a1 b1 -> \case TakePart a2 b2 -> sameItems a1 a2 && sameItems b1 b2; _ -> False
