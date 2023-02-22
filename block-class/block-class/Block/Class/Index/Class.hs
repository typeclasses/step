module Block.Class.Index.Class where

import Essentials

import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty (..))
import Integer (Positive)
import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Positional.Class (Positional (..))
import Prelude ((-))

import qualified Data.List.NonEmpty as NonEmpty

class (Singleton x xs, Positional xs) => Index x xs where

    {-| Get the item at a particular position

    Returns 'Nothing' if the position is greater than 'length'.

    The first item's position is 1. (Please take note, because
    this is unconventional.) -}
    at :: End -> Positive -> xs -> Maybe x

instance Index x (NonEmpty x) where

    at :: End -> Positive -> NonEmpty x -> Maybe x
    at Front 1 = \(x :| _) -> Just x
    at Front n = \(_ :| xs) -> go (n - 1) xs
      where
        go _ [] = Nothing
        go 1 (x : _) = Just x
        go i (_ : xs) = go i xs
    at Back n = NonEmpty.reverse >>> at Front n
