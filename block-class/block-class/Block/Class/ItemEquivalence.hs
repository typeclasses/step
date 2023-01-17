module Block.Class.ItemEquivalence where

import Essentials
import Block.Class.Block

import Data.Ord (compare, Ordering (..))
import Prelude (error)

{-| An equivalence on characters, expressed as an equivalence on blocks

It must be the case that blocks /a/ and /b/ are equivalent iff the length
of /a/ and /b/ is /l/ and /a[i] ~ b[i]/ for all /i = [1 .. l]/ according
to the character equivalence. -}
newtype BlockCharacterEquivalence c =
    BlockCharacterEquivalence{ blocksEquivalent :: c -> c -> Bool }

stripEitherPrefix :: Block c =>
    BlockCharacterEquivalence c -> c -> c -> StripEitherPrefix c
stripEitherPrefix eq a b = case compare (length a) (length b) of
    EQ -> if blocksEquivalent eq a b
            then StripEitherPrefixAll
            else StripEitherPrefixFail
    LT -> case split (length a) b of
        Split a' b' -> if blocksEquivalent eq a a'
            then IsPrefixOf{ commonPart = a', extraPart = b' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
    GT -> case split (length b) a of
        Split b' a' -> if blocksEquivalent eq b b'
            then IsPrefixedBy{ commonPart = b', extraPart = a' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"

data StripEitherPrefix c =
    StripEitherPrefixAll
  | StripEitherPrefixFail
  | IsPrefixOf   { commonPart :: c, extraPart :: c }
  | IsPrefixedBy { commonPart :: c, extraPart :: c }
