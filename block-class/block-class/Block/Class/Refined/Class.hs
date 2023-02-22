module Block.Class.Refined.Class where

import Essentials

import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Prelude (error)

import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable

class Refined nul xs | xs -> nul where

    refine :: nul -> Maybe xs

    generalize :: xs -> nul

    {-| Defined only where 'refine' produces 'Just' -}
    assume :: nul -> xs
    assume = refine >>> Maybe.fromJust

    {-# minimal refine, generalize #-}

instance Refined [x] (NonEmpty x) where

    refine :: [x] -> Maybe (NonEmpty x)
    refine = nonEmpty

    generalize :: NonEmpty x -> [x]
    generalize = Foldable.toList

    assume :: [x] -> NonEmpty x
    assume (x : xs) = x :| xs
    assume [] = error "Block.assume NonEmpty"
