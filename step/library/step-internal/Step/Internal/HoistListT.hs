{-# language Trustworthy #-}

module Step.Internal.HoistListT
  (
    hoistListT,
  )
  where

import Step.Internal.Dependencies

import ListT

-- https://github.com/Gabriella439/Haskell-List-Transformer-Library/issues/25

hoistListT :: Monad m => (forall a. m a -> n a) -> ListT m b -> ListT n b
hoistListT f ListT{ next } = ListT{ next = f (fmap (hoistStep f) next) }

hoistStep :: Monad m => (forall a. m a -> n a) -> Step m b -> Step n b
hoistStep f = \case{ Nil -> Nil; Cons x xs -> Cons x (hoistListT f xs) }
