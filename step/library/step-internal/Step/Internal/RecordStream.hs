module Step.Internal.RecordStream where

import Monad (Monad, return)

import MonadTrans (lift)

import State (StateT, modify')

import ListT (ListT (ListT))
import qualified ListT

recordStream :: Monad m => (a -> s -> s) -> ListT m a -> ListT (StateT s m) a
recordStream add = r
  where
    r xs = ListT do
      step <- lift (ListT.next xs)
      case step of
          ListT.Nil -> return ListT.Nil
          ListT.Cons x xs' -> do
              modify' (add x)
              return (ListT.Cons x (r xs'))
