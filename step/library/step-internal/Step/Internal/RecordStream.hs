{-# language Trustworthy #-}

module Step.Internal.RecordStream where

import Step.Internal.Dependencies

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
