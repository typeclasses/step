{-# language FlexibleInstances, FunctionalDependencies #-}

module Step.Internal.Contravariant where

import Contravariant (Predicate (..))
import qualified Contravariant

class Contravariant a1 a2 b1 b2 | a1 -> b1, a1 b2 -> a2 where
    contramap :: (b2 -> b1) -> a1 -> a2

instance Contravariant (Predicate a) (Predicate b) a b where
    contramap = Contravariant.contramap
