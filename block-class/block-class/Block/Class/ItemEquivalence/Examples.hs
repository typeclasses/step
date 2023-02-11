module Block.Class.ItemEquivalence.Examples where

import Essentials

import Block.Class.ItemEquivalence.Type

equality :: Eq xs => ItemEquivalence xs
equality = ItemEquivalence \(a, b) -> a == b
