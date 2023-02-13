module Block.Class.Refined.Class where

import Essentials

import Block.Class.Block.Class (Block)
import Block.Class.Refined.Family (Nullable)

class (Block c, Monoid (Nullable c)) => Refined c where

    refine :: Nullable c -> Maybe c

    generalize :: c -> Nullable c

    {-| Defined only where 'refine' produces 'Just' -}
    assume :: Nullable c -> c
