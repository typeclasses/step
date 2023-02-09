module Block.Singleton.Utilities where

import Essentials

import Block.Item (Item)
import Block.Singleton.Class (Singleton (..))
import Block.Singleton.Types (Pop (..))
import Block.End (End (..))

unpop :: (Singleton xs, Item xs ~ x) =>
    End -> Pop xs -> xs
unpop s (Pop x xm) = case xm of
    Nothing -> singleton x
    Just xs -> push s x xs

head :: (Singleton xs, Item xs ~ x) =>
    xs -> x
head = item . pop Front
