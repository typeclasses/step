module Stratoparsec.Buffer where

import Mono (MonoFoldable)
import ListT (ListT)

import qualified Seq
import qualified Mono
import qualified ListT

data Buffer chunk = Buffer{ chunks :: Seq chunk, size :: Natural }

instance Semigroup (Buffer chunk) where
    a <> b = Buffer{ chunks = chunks a <> chunks b,
                     size = size a + size b }

singleton :: MonoFoldable a => a -> Buffer a
singleton x =
    Buffer{ chunks = Seq.singleton x, size = fromIntegral $ Mono.olength x }

isEmpty :: Buffer chunk -> Bool
isEmpty = (== 0) . size

empty :: Buffer chunk
empty = Buffer{ chunks = Seq.empty, size = 0 }

toListT :: Monad m => Buffer a -> ListT m a
toListT = ListT.select . chunks
