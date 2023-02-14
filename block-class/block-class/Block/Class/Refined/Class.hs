module Block.Class.Refined.Class where

import Essentials

import Block.Class.Block.Class (Block)
import Block.Class.Refined.Family (Nullable)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Foldable (toList)
import Prelude (error)

class (Block c, Monoid (Nullable c)) => Refined c where

    refine :: Nullable c -> Maybe c

    generalize :: c -> Nullable c

    {-| Defined only where 'refine' produces 'Just' -}
    assume :: Nullable c -> c

instance Refined (NonEmpty xs) where
    refine = nonEmpty
    generalize = toList
    assume (x : xs) = x :| xs
    assume [] = error "Block.assume NonEmpty"
