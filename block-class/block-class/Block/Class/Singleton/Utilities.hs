module Block.Class.Singleton.Utilities
  (
    {- * Utilities -} unpop, terminal, first, last, pushMaybe,
  )
  where

import Essentials

import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Singleton.Types (Pop (..))
import Block.Class.End (End (..))

{-| The inverse of 'pop' -}
unpop :: (Singleton x xs) =>
    End -- ^ 'Front' or 'Back'
    -> Pop x xs -- ^ Division of a block into item and remainder
    -> xs -- ^ Item and remainder concatenated back together
unpop s (Pop x xm) = case xm of
    Nothing -> singleton x
    Just xs -> push s x xs

terminal :: (Singleton x xs) => End -> xs -> x
terminal e = item . pop e

{-| The item at the 'Front' of a block -}
first :: (Singleton x xs) =>
    xs -- ^ A block
    -> x -- ^ The block's first item
first = terminal Front

{-| The item at the 'Back' of a block -}
last :: (Singleton x xs) =>
    xs -- ^ A block
    -> x -- ^ The block's first item
last = terminal Back

pushMaybe :: (Singleton x xs) => End -> Maybe x -> Maybe xs -> Maybe xs
pushMaybe end (Just x) (Just xs)  =  Just (push end x xs)
pushMaybe _   Nothing  (Just xs)  =  Just xs
pushMaybe _   (Just x) Nothing    =  Just (singleton x)
pushMaybe _   Nothing  Nothing    =  Nothing
