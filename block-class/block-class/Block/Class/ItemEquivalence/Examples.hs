module Block.Class.ItemEquivalence.Examples
  (
    {- * Item equivalences -} equality, itemEquality,
  )
  where

import Essentials

import Block.Class.ItemEquivalence.Types
import Block.Class.ItemEquality.Class

{-| Equivalence based on 'Eq' -}
equality :: Eq xs => ItemEquivalence xs
equality = ItemEquivalence \(a, b) -> a == b

itemEquality :: ItemEquality xs => ItemEquivalence xs
itemEquality = ItemEquivalence \(a, b) -> sameItems a b
