module Block.Class.ItemEquality.Utilities
  (
    {- * Utilities -} sameItemsTake, sameItemsPop, foldableEqOn,
  )
  where

import Essentials
import Block.Class.ItemEquality.Class
import Block.Class.Positional.Types
import Block.Class.Singleton.Types

import Data.Bool ((&&))
import Data.Function (on)

import qualified Data.Foldable as Foldable

sameItemsTake :: ItemEquality xs => Take xs -> Take xs -> Bool
sameItemsTake = \case
    TakeAll -> \case TakeAll -> True; _ -> False
    TakeInsufficient s1 -> \case TakeInsufficient s2 -> s1 == s2; _ -> False
    TakePart a1 b1 -> \case TakePart a2 b2 -> sameItems a1 a2 && sameItems b1 b2; _ -> False

sameItemsPop :: Eq x => ItemEquality xs => Pop x xs -> Pop x xs -> Bool
sameItemsPop (Pop x1 xs1) (Pop x2 xs2) = x1 == x2 && foldableEqOn sameItems xs1 xs2

foldableEqOn :: Foldable f => (x -> x -> Bool) -> f x -> f x -> Bool
foldableEqOn f = listEqOn f `on` Foldable.toList

listEqOn :: (x -> x -> Bool) -> [x] -> [x] -> Bool
listEqOn _ [] [] = True
listEqOn _ [] (_ : _) = False
listEqOn _ (_ : _) [] = False
listEqOn f (a : as) (b : bs) = f a b && listEqOn f as bs
