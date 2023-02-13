module Block.Class.Refined.Utilities where

import Essentials

import Block.Class.Refined.Class (Refined, generalize)
import Block.Class.Refined.Family (Nullable)

import qualified Data.Foldable as Foldable

fold :: Refined xs => [xs] -> Nullable xs
fold = fmap generalize >>> Foldable.fold
