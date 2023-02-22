module Block.Class.Concat.Class
  (
    {- * Class -} Concat (..),
  )
  where

import Essentials

import Data.List.NonEmpty (NonEmpty)
import Block.Class.End (End (..))

import qualified Data.Semigroup as Semigroup
import qualified Data.List.NonEmpty as NonEmpty

class Concat xs where
    (++) :: xs -> xs -> xs
    concat :: End -> NonEmpty xs -> xs

instance Concat (NonEmpty xs) where

    (++) :: NonEmpty xs -> NonEmpty xs -> NonEmpty xs
    (++) = (Semigroup.<>)

    concat :: End -> NonEmpty (NonEmpty xs) -> NonEmpty xs
    concat Front = Semigroup.sconcat
    concat Back = NonEmpty.reverse >>> Semigroup.sconcat
