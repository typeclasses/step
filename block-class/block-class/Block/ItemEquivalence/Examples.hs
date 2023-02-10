module Block.ItemEquivalence.Examples where

import Essentials

import Block.ItemEquivalence.Type

equality :: Eq xs => ItemEquivalence xs
equality = ItemEquivalence \(a, b) -> a == b
