module Block.Class.Enumerate.Class
  (
    {- * Class -} Enumerate (..),
  )
  where

import Essentials

import Block.Class.ItemEquality.Class (ItemEquality)
import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Foldable

class (Eq x, ItemEquality xs) => Enumerate x xs | xs -> x where

    toNonEmpty :: End -> xs -> NonEmpty x

    foldItems :: End -> (x -> a) -> (a -> x -> a) -> xs -> a
    foldItems end initial step = toNonEmpty end >>> foldItems end initial step

instance (Eq x) => Enumerate x (NonEmpty x) where

    toNonEmpty :: End -> NonEmpty x -> NonEmpty x
    toNonEmpty = \case Front -> id; Back -> NonEmpty.reverse

    foldItems :: Eq x => End -> (x -> a) -> (a -> x -> a) -> NonEmpty x -> a
    foldItems end initial step =
        toNonEmpty end >>> \(x :| xs) ->
        Foldable.foldl' step (initial x) xs
