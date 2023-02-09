module Block.BiPrefix.Utilities where

import Essentials

import Data.Function (on)
import Block.Positional (Split (..), Amount (..), Positional (..))
import Block.ItemEquivalence.Type (ItemEquivalence, blocksEquivalent)
import Data.Ord (compare, Ordering (..))
import Prelude (error)
import Block.End (End (..))
import Block.BiPrefix.Types (BiPrefix (..), Which (..))

{-| Given a pair of blocks, determine whether either is a prefix
    of the other, according to an item equivalence -}
biPrefix :: Positional xs =>
    ItemEquivalence xs
    -> (xs, xs)
    -> BiPrefix xs
biPrefix (blocksEquivalent -> same) pair = case whichIsShorter pair of
    Nothing -> if same pair then Same else NoPrefixRelation
    Just theShorter -> case split (Amount Front (length short)) long of
        Split prefix suffix ->
            if same (prefix, short)
            then IsPrefix theShorter prefix suffix
            else NoPrefixRelation
        SplitInsufficient{} -> error "biPrefix"
      where
        (short, long) = case theShorter of First -> pair; Second -> swap pair

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

whichIsShorter :: Positional xs => (xs, xs) -> Maybe Which
whichIsShorter (a, b) = case (compare `on` length) a b of
    EQ -> Nothing
    LT -> Just First
    GT -> Just Second