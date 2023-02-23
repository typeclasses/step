module Block.Class.ItemEquivalence.Type where

import Essentials

{-| An equivalence on @('Block.Class.Item' xs)@, expressed as an equivalence
on @xs@ that must satisfy the following condition:

There must exist a predicate

@eq :: (Item xs, Item xs) -> Bool@

such that

@'equivalentItems' (a, b)@

is the same as

@
('Block.Class.length' a == 'Block.Class.length' b)
    && ('Data.Foldable.all' eq ('Data.NonEmpty.zip' ('Block.Class.toNonEmpty' a) ('Block.Class.toNonEmpty' b)))
@

The point here is that we want to work with efficiently packed string
types like @(Text1)@ but still be able to reason about them as if they
were @(NonEmpty Char])@. -}
newtype ItemEquivalence xs =
    ItemEquivalence{ equivalentItems :: (xs, xs) -> Bool }
