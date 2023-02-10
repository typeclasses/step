module Block.Class.Trivializable where

import Essentials
import Block.Block.Class

import qualified Data.Foldable as Foldable

type family Nullable (c :: Type) :: Type

class (Block c, Monoid (Nullable c)) => Trivializable c where

    refine :: Nullable c -> Maybe c

    generalize :: c -> Nullable c

    {-| Defined only where 'refine' produces 'Just' -}
    assume :: Nullable c -> c

fold :: Trivializable c => [c] -> Nullable c
fold = fmap generalize >>> Foldable.fold
