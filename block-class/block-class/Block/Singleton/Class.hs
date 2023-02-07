module Block.Singleton.Class where

import Essentials

import Block.Item (Item)
import Block.Singleton.Types (Pop)
import Block.End (End)

class (Semigroup xs) => Singleton xs where

    {-| A block with a single item -}
    singleton :: (Item xs ~ x) =>
        x -> xs

    {-| Cut a block into two parts: The extreme item from the front/back,
        and maybe a remainder -}
    pop :: End -> xs -> Pop xs

    {-| Add one item onto the front/back of a block -}
    push :: (Item xs ~ x) =>
        End -> x -> xs -> xs
