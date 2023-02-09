module Block.Singleton.Types where

import Essentials

import Block.Item (Item)

data Pop xs = Pop
    { item :: Item xs
    , remainder :: Maybe xs
    }
