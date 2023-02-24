module Block.Class.Enumerate.Class
  (
    {- * Class -} Enumerate (..),
  )
  where

import Essentials

import Block.Class.ItemEquality.Class (ItemEquality)
import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty (..))
import Block.Class.State.Types (State)
import Block.Class.State.Utilities (execState)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Foldable

class (Eq x, ItemEquality xs) => Enumerate x xs | xs -> x where

    toNonEmpty :: End -> xs -> NonEmpty x

    foldItems :: End -> (x -> a) -> (x -> State a ()) -> xs -> a

instance (Eq x) => Enumerate x (NonEmpty x) where

    toNonEmpty :: End -> NonEmpty x -> NonEmpty x
    toNonEmpty = \case Front -> id; Back -> NonEmpty.reverse

    foldItems :: End -> (x -> a) -> (x -> State a ()) -> NonEmpty x -> a
    foldItems end initial step =
        toNonEmpty end >>> \(x0 :| xs) ->
        Foldable.foldl' (\s x -> step x & execState s) (initial x0) xs
