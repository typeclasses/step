{- |

Description: The 'InfiniteStream' interface, like 'TerminableStream' but for lists that never end

-}

module SupplyChain.Interface.InfiniteStream
  (
    {- * Interface -} InfiniteStream (..),
    {- * Vendors -} iterate, concatMap, concat,
  )
  where

import SupplyChain

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function (($), flip, id)
import Data.Kind (Type)


data InfiniteStream item response =
    (response ~ item) => Next
        -- ^ The next item from a non-terminating input stream

type InfiniteStream :: Type -> Interface


iterate :: forall up a action param.
    a -> (a -> a) -> Vendor up (InfiniteStream a) action param

iterate = flip it
  where
    it f = go
      where
        go :: a -> Vendor up (InfiniteStream a) action param
        go x = Vendor \Next -> pure $ Supply x (go (f x))


concatMap :: forall a b action param.
    (a -> [b]) -> Vendor (InfiniteStream a) (InfiniteStream b) action param

concatMap f = go []
  where
    go :: [b] -> Vendor (InfiniteStream a) (InfiniteStream b) action param
    go bs = Vendor \Next -> case bs of
        b : bs' -> pure $ Supply b (go bs')
        [] -> order Next >>= \a -> offer (go (f a)) Next


concat :: forall a action param.
    Vendor (InfiniteStream [a]) (InfiniteStream a) action param

concat = concatMap id
