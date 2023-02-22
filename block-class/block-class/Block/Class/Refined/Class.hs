module Block.Class.Refined.Class where

import Essentials

import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Foldable (toList)
import Prelude (error)

class Refined nul xs | xs -> nul where

    refine :: nul -> Maybe xs

    generalize :: xs -> nul

    {-| Defined only where 'refine' produces 'Just' -}
    assume :: nul -> xs

instance Refined [x] (NonEmpty x) where
    refine = nonEmpty
    generalize = toList
    assume (x : xs) = x :| xs
    assume [] = error "Block.assume NonEmpty"
