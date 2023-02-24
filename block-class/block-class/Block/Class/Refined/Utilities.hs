module Block.Class.Refined.Utilities
  (
    {- * Utilities -} concatRefined,
  )
  where

import Essentials

import Block.Class.Refined.Class (Refined, generalize)

import qualified Data.Foldable as Foldable

concatRefined :: (Monoid nul, Refined nul xs) => [xs] -> nul
concatRefined = fmap generalize >>> Foldable.fold
