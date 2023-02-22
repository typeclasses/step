module Block.Class.Concat.Utilities where

import Block.Class.Concat.Class

import Data.Function (flip)
import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Block.Class.End as End

append :: Concat xs => End -> xs -> (xs -> xs)
append Front = (++)
append Back = flip (++)

{-| One possible implementation of 'concat', written in terms of '++' -}
concatRecursively :: Concat xs => End -> NonEmpty xs -> xs
concatRecursively e (x :| xs) = go x xs
  where
    go acc [] = acc
    go acc (y : ys) = go (append (End.opposite e) y acc) ys
