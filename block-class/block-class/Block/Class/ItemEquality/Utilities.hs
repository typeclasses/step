module Block.Class.ItemEquality.Utilities
  (
    {- * Utilities -}
    sameItemsTake, sameItemsPop, sameItemsPivot, sameItemsSpan,
    foldableEqOn,
  )
  where

import Essentials
import Block.Class.ItemEquality.Class
import Block.Class.Positional.Types
import Block.Class.Singleton.Types
import Block.Class.Search.Types

import Data.Bool ((&&))
import Data.Function (on)

import qualified Data.Foldable as Foldable

sameItemsFoldable :: ItemEquality xs => Foldable f => f xs -> f xs -> Bool
sameItemsFoldable = foldableEqOn sameItems

sameItemsTake :: ItemEquality xs => Take xs -> Take xs -> Bool
sameItemsTake = \case
    TakeAll -> \case TakeAll -> True; _ -> False
    TakeInsufficient s1 -> \case TakeInsufficient s2 -> s1 == s2; _ -> False
    TakePart a1 b1 -> \case TakePart a2 b2 -> sameItems a1 a2 && sameItems b1 b2; _ -> False

sameItemsPop :: Eq x => ItemEquality xs => Pop x xs -> Pop x xs -> Bool
sameItemsPop (Pop x1 xs1) (Pop x2 xs2) = x1 == x2 && sameItemsFoldable xs1 xs2

sameItemsPivot :: Eq found => ItemEquality xs =>
    Pivot found xs -> Pivot found xs -> Bool
sameItemsPivot (Pivot a1 b1 c1) (Pivot a2 b2 c2) =
    sameItemsFoldable a1 a2 && b1 == b2 && sameItemsFoldable c1 c2

sameItemsSpan :: ItemEquality xs => Span xs -> Span xs -> Bool
sameItemsSpan = \case
    SpanNone -> \case SpanNone -> True; _ -> False
    SpanAll -> \case SpanAll -> True; _ -> False
    SpanPart a1 b1 -> \case SpanPart a2 b2 -> sameItems a1 a2 && sameItems b1 b2; _ -> False

foldableEqOn :: Foldable f => (x -> x -> Bool) -> f x -> f x -> Bool
foldableEqOn f = listEqOn f `on` Foldable.toList

listEqOn :: (x -> x -> Bool) -> [x] -> [x] -> Bool
listEqOn _ [] [] = True
listEqOn _ [] (_ : _) = False
listEqOn _ (_ : _) [] = False
listEqOn f (a : as) (b : bs) = f a b && listEqOn f as bs
