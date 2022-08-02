{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module Step.Internal.Contravariant
  (
    Contravariant (..),
    contravoid, contraconst,
  )
  where

import Contravariant (Predicate (..))
import qualified Contravariant

class Contravariant a1 a2 b1 b2 | a1 -> b1, a2 -> b2, a1 b2 -> a2, a2 b1 -> a1 where
    contramap :: (b2 -> b1) -> a1 -> a2

instance Contravariant (Predicate a) (Predicate b) a b where
    contramap = Contravariant.contramap

contravoid :: Contravariant a1 a2 () b2 => a1 -> a2
contravoid = contraconst ()

contraconst :: Contravariant a1 a2 b1 b2 => b1 -> a1 -> a2
contraconst x = contramap \_ -> x
