module Block.Class.Concat.Class
  (
    {- * Class -} Concat (..),
  )
  where

import Essentials

import Data.List.NonEmpty (NonEmpty)
import Block.Class.End (End (..))
import Block.Class.ItemEquality.Class (ItemEquality (..))

import qualified Data.Semigroup as Semigroup
import qualified Data.List.NonEmpty as NonEmpty

class ItemEquality xs => Concat xs where
    (++) :: xs -> xs -> xs
    concat :: End -> NonEmpty xs -> xs

instance (Eq x) => Concat (NonEmpty x) where

    (++) :: NonEmpty x -> NonEmpty x -> NonEmpty x
    (++) = (Semigroup.<>)

    concat :: End -> NonEmpty (NonEmpty x) -> NonEmpty x
    concat Front = Semigroup.sconcat
    concat Back = NonEmpty.reverse >>> Semigroup.sconcat
