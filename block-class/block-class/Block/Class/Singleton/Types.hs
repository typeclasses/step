module Block.Class.Singleton.Types where

import Essentials

import Block.Class.Item (Item)

data Pop xs = Pop
    { item :: Item xs
    , remainder :: Maybe xs
    }
