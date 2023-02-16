module Block.Class.Singleton.Utilities where

import Essentials

import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Singleton.Types (Pop (..))
import Block.Class.End (End (..))

{-| The inverse of 'pop' -}
unpop :: Singleton x xs =>
    End -- ^ 'Front' or 'Back'
    -> Pop x xs -- ^ Division of a block into item and remainder
    -> xs -- ^ Item and remainder concatenated back together
unpop s (Pop x xm) = case xm of
    Nothing -> singleton x
    Just xs -> push s x xs

{-| The item at the 'Front' of a block -}
head :: (Singleton x xs) =>
    xs -- ^ A block
    -> x -- ^ The block's first item
head = item . pop Front
