module Block.Class.Singleton.Types where

import Essentials

import Block.Class.Item (Item)

data Pop x xs = (Item xs ~ x) => Pop
    { item :: x
    , remainder :: Maybe xs
    }

deriving stock instance (Eq x, Eq xs) => Eq (Pop x xs)
deriving stock instance (Ord x, Ord xs) => Ord (Pop x xs)
deriving stock instance (Show x, Show xs) => Show (Pop x xs)
