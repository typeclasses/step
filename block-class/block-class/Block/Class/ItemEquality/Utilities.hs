module Block.Class.ItemEquality.Utilities
  (
    {- * Utilities -} foldableEqOn,
  )
  where

import Essentials

import Data.Bool ((&&))
import Data.Function (on)

import qualified Data.Foldable as Foldable

foldableEqOn :: Foldable f => (x -> x -> Bool) -> f x -> f x -> Bool
foldableEqOn f = listEqOn f `on` Foldable.toList

listEqOn :: (x -> x -> Bool) -> [x] -> [x] -> Bool
listEqOn _ [] [] = True
listEqOn _ [] (_ : _) = False
listEqOn _ (_ : _) [] = False
listEqOn f (a : as) (b : bs) = f a b && listEqOn f as bs
