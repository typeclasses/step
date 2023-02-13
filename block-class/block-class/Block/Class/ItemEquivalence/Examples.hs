module Block.Class.ItemEquivalence.Examples where

import Essentials

import Block.Class.ItemEquivalence.Type

{-| Equivalence based on 'Eq' -}
equality :: Eq xs => ItemEquivalence xs
equality = ItemEquivalence \(a, b) -> a == b
