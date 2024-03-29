{-| The 'InfiniteStream' interface, like 'TerminableStream'
    but for lists that never end -}
module SupplyChain.Interface.InfiniteStream
  (
    {- * Interface -} InfiniteStream (..),
    {- * Vendors -} forever, iterate, concatMap, concat,
  )
  where

import Essentials
import SupplyChain

data InfiniteStream item response =
    (response ~ item) => Next
        -- ^ The next item from a non-terminating input stream

forever :: Job up action a -> Vendor up (InfiniteStream a) action
forever j = SupplyChain.loop' \Next -> j

iterate :: forall up a action.
    a -> (a -> a) -> Vendor up (InfiniteStream a) action
iterate = \x f -> it f x
  where
    it f = go
      where
        go :: a -> Vendor up (InfiniteStream a) action
        go x = Vendor \Next -> pure $ Referral x (go (f x))

concatMap :: forall a b action.
    (a -> [b]) -> Vendor (InfiniteStream a) (InfiniteStream b) action
concatMap f = go []
  where
    go :: [b] -> Vendor (InfiniteStream a) (InfiniteStream b) action
    go bs = Vendor \Next -> case bs of
        b : bs' -> pure $ Referral b (go bs')
        [] -> order Next >>= \a -> handle (go (f a)) Next

concat :: forall a action.
    Vendor (InfiniteStream [a]) (InfiniteStream a) action
concat = concatMap id
