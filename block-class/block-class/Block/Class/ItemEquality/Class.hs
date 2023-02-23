module Block.Class.ItemEquality.Class where

import Essentials

import Data.List.NonEmpty (NonEmpty (..))

class ItemEquality xs where
    sameItems :: xs -> xs -> Bool

instance (Eq x) => ItemEquality (NonEmpty x) where

    sameItems :: NonEmpty x -> NonEmpty x -> Bool
    sameItems = (==)
