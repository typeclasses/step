module Block.ItemEquivalence.Type where

import Essentials

{-| An equivalence on characters, expressed as an equivalence on blocks

It must be the case that blocks /a/ and /b/ are equivalent iff the length
of /a/ and /b/ is /l/ and /a[i] ~ b[i]/ for all /i = [1 .. l]/ according
to the character equivalence.

The point here is that we want to work with efficiently packed string types
like @Text@ but still be able to reason about them as if they were @[Char]@. -}
newtype ItemEquivalence c =
    ItemEquivalence{ blocksEquivalent :: (c, c) -> Bool }
