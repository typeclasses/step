{-# language RankNTypes, Trustworthy #-}

module Step.Internal.ChangeBaseListT where

import Function (($))
import Functor (Functor, (<&>))
import Monad ((>>=))

import ListT (ListT (ListT))
import qualified ListT

changeBaseListT :: Functor m2 => (forall x. m1 x -> m2 x) -> ListT m1 a -> ListT m2 a
changeBaseListT f xs = ListT $ f (ListT.next xs) <&> \case
    ListT.Nil -> ListT.Nil
    ListT.Cons x r -> ListT.Cons x (changeBaseListT f r)
