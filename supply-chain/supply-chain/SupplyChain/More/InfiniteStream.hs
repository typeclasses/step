module SupplyChain.More.InfiniteStream where

import SupplyChain

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function (($), flip, id)
import Data.Functor (Functor)
import Data.Kind (Type)


data InfiniteStream item response =
    (response ~ item) => Next
        -- ^ The next item from a non-terminating input stream

type InfiniteStream :: Type -> Interface


iterate :: forall up a action. Functor action =>
    a -> (a -> a) -> Vendor up (InfiniteStream a) action

iterate = flip it
  where
    it f = go
      where
        go :: a -> Vendor up (InfiniteStream a) action
        go x = Vendor \Next -> pure $ x :-> go (f x)


concatMap :: forall a b action. Functor action =>
    (a -> [b]) -> Vendor (InfiniteStream a) (InfiniteStream b) action

concatMap f = go []
  where
    go :: [b] -> Vendor (InfiniteStream a) (InfiniteStream b) action
    go bs = Vendor \Next -> case bs of
        b : bs' -> pure $ b :-> go bs'
        [] -> order Next >>= \a -> offer (go (f a)) Next


concat :: forall a action. Functor action =>
    Vendor (InfiniteStream [a]) (InfiniteStream a) action

concat = concatMap id
