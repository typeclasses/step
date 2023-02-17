module Block.Class.Refined.Utilities where

import Essentials

import Block.Class.Refined.Class (Refined, generalize)

import qualified Data.Foldable as Foldable

concat :: (Monoid nul, Refined x nul xs) => [xs] -> nul
concat = fmap generalize >>> Foldable.fold
