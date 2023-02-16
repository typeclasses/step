module Block.Class.Refined.Utilities where

import Essentials

import Block.Class.Refined.Class (Refined, generalize)

import qualified Data.Foldable as Foldable

fold :: Refined x nul xs => [xs] -> nul
fold = fmap generalize >>> Foldable.fold
