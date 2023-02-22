module Block.Class.Singleton.Class where

import Essentials

import Block.Class.Singleton.Types (Pop (..))
import Block.Class.End (End (..))
import Block.Class.Concat.Class (Concat (..))

import Data.List.NonEmpty (NonEmpty (..), nonEmpty, reverse)

class Concat xs => Singleton x xs | xs -> x where

    {-| A block with a single item -}
    singleton ::
        x -- ^ An item
        -> xs -- ^ A block consisting of exactly one item

    {-| Cut a block into two parts: The item from the front/back,
        and maybe a remainder -}
    pop ::
        End -- ^ 'Front' or 'Back'
        -> xs -- ^ The block
        -> Pop x xs -- ^ Division of the block into item and remainder

    {-| Add one item onto the front/back of a block -}
    push ::
        End -- ^ 'Front' or 'Back'
        -> x -- ^ An item
        -> xs -- ^ A block
        -> xs -- ^ A new block with the item appended to it

instance Singleton x (NonEmpty x) where

    singleton :: x -> NonEmpty x
    singleton = (:| [])

    pop :: End -> NonEmpty x -> Pop x (NonEmpty x)
    pop = \case
        Front -> \(x :| xs) -> Pop x (nonEmpty xs)
        Back -> \xs ->
            let p = pop Front (reverse xs)
            in p{ remainder = reverse <$> remainder p }

    push :: End -> x -> NonEmpty x -> NonEmpty x
    push = \case
        Front -> \x (y :| ys) -> x :| y : ys
        Back -> \x xs -> reverse (push Front x (reverse xs))
