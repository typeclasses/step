module Block.Class.Trivializable where

import Essentials
import Block.Class.Block

type family Nullable (c :: Type) :: Type

class (Block c, Monoid (Nullable c)) => Trivializable c where

    refine :: Nullable c -> Maybe c

    generalize :: c -> Nullable c

concatTrivialize :: Trivializable c => [c] -> Nullable c
concatTrivialize = concatMaybe >>> maybe mempty generalize
