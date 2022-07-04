module Variado.Monad.Class where

import BasePrelude (Functor)
import Kind (Type)

class PolyMonad m1 m2 where
    type Join m1 m2 :: Type -> Type
    join :: (Functor m1, Functor m2) => m1 (m2 a) -> (Join m1 m2) a

-- class PolyMonadKey k where
--     type KeyJoin (k1 :: k) (k2 :: k) :: k

-- newtype Keyed k a = Keyed a

-- instance PolyMonadKey k => PolyMonad (Keyed (k1 :: k)) (Keyed (k2 :: k)) where
--     type Join (Keyed k1) (Keyed k2) = Keyed (KeyJoin k1 k2)
