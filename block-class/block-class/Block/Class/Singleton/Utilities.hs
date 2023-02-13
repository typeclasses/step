module Block.Class.Singleton.Utilities where

import Essentials

import Block.Class.Item (Item)
import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Singleton.Types (Pop (..))
import Block.Class.End (End (..))

{-| The inverse of 'pop' -}
unpop :: (Singleton xs, Item xs ~ x) =>
    End -> Pop xs -> xs
unpop s (Pop x xm) = case xm of
    Nothing -> singleton x
    Just xs -> push s x xs

head :: (Singleton xs, Item xs ~ x) =>
    xs -> x
head = item . pop Front

pushMaybes :: (Singleton xs, Item xs ~ x) =>
    End -> Maybe x -> Maybe xs -> Maybe xs
pushMaybes s = maybe id (\x -> fmap (push s x))
