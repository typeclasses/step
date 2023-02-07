module Block.Singleton.Types where

import Essentials

import Block.Item (Item)

data Pop xs = Pop
    { popItem :: Item xs
    , popRemainder :: Maybe xs
    }
