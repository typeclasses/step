module Block.Class.Index.Class where

import Essentials

import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty (..))
import Integer (Positive)
import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.Positional.Class (Positional (..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Integer.Positive as Positive

class (Singleton x xs, Positional xs) => Index x xs where

    {-| Get the item at a particular position

    Returns 'Nothing' if the position is greater than 'length'.

    The first item's position is 1. (Please take note, because
    this is unconventional.) -}
    at :: End -> Positive -> xs -> Maybe x

instance Index x (NonEmpty x) where

    at :: End -> Positive -> NonEmpty x -> Maybe x
    at Front n = case Positive.fromNatural (Positive.subtractOne n) of
        Nothing -> \(x :| _) -> Just x
        Just n' -> \(_ :| xs) -> go n' xs
      where
        go i xs = case xs of
            [] -> Nothing
            (x : xs') ->
                case Positive.fromNatural (Positive.subtractOne i) of
                    Nothing -> Just x
                    Just i' -> go i' xs'
    at Back n = NonEmpty.reverse >>> at Front n
