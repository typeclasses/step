module Block.Class.Singleton.Types where

import Essentials

data Pop x xs = Pop{ item :: x, remainder :: Maybe xs }

deriving stock instance (Eq x, Eq xs) => Eq (Pop x xs)
deriving stock instance (Ord x, Ord xs) => Ord (Pop x xs)
deriving stock instance (Show x, Show xs) => Show (Pop x xs)
