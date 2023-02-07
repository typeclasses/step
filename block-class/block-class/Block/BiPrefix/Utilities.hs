module Block.BiPrefix.Utilities where

import Block.Class.Block
import Block.Positional

import Block.ItemEquivalence.Type (ItemEquivalence, blocksEquivalent)
import Data.Ord (compare, Ordering (..))
import Prelude (error)
import Block.End (End (..))
import Block.BiPrefix.Types (BiPrefix (..))

{-| Given a pair of blocks, determine whether either is a prefix
    of the other, according to an item equivalence -}
biPrefix :: Block xs =>
    ItemEquivalence xs
    -> (xs, xs)
    -> BiPrefix xs
biPrefix eq (a, b) = case compare (length a) (length b) of

    EQ -> if blocksEquivalent eq a b
            then Same
            else NoPrefixRelation

    LT -> case split (Amount Front (length a)) b of
        Split (Prefix a') (Suffix b') -> if blocksEquivalent eq a a'
            then IsPrefixOf{ commonPart = a', extraPart = b' }
            else NoPrefixRelation
        SplitInsufficient{} -> error "biPrefix"

    GT -> case split (Amount Front (length b)) a of
        Split (Prefix b') (Suffix a') -> if blocksEquivalent eq b b'
            then IsPrefixedBy{ commonPart = b', extraPart = a' }
            else NoPrefixRelation
        SplitInsufficient{} -> error "biPrefix"
