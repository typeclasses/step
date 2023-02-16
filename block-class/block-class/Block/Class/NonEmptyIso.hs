module Block.Class.NonEmptyIso where

import Data.List.NonEmpty (NonEmpty)

class NonEmptyIso x xs | xs -> x where
    toNonEmpty :: xs -> NonEmpty x
    fromNonEmpty :: NonEmpty x -> xs

instance NonEmptyIso x (NonEmpty x) where
    toNonEmpty x = x
    fromNonEmpty x = x
