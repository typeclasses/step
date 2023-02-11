module Block.Class.Singleton.Class where

import Essentials

import Block.Class.Item (Item)
import Block.Class.Singleton.Types (Pop (..))
import Block.Class.End (End (..))

import Data.List.NonEmpty (NonEmpty (..), nonEmpty, reverse)

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

instance Singleton (NonEmpty x) where

    singleton :: x -> NonEmpty x
    singleton = (:| [])

    pop :: End -> NonEmpty x -> Pop (NonEmpty x)
    pop = \case
        Front -> \(x :| xs) -> Pop x (nonEmpty xs)
        Back -> \xs ->
            let p = pop Front (reverse xs)
            in p{ remainder = reverse <$> remainder p }

    push :: End -> x -> NonEmpty x -> NonEmpty x
    push = \case
        Front -> \x (y :| ys) -> x :| y : ys
        Back -> \x xs -> reverse (push Front x (reverse xs))
