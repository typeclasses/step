module Block.Class.NonEmptyIso where

import Block.Class.Item

import Data.List.NonEmpty (NonEmpty)

class NonEmptyIso xs where
    toNonEmpty :: xs -> NonEmpty (Item xs)
    fromNonEmpty :: NonEmpty (Item xs) -> xs
