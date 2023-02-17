module Block.Class.Singleton.Types where

import Essentials

data Pop x xs = Pop{ item :: x, remainder :: Maybe xs }
    deriving stock (Eq, Ord, Show)
