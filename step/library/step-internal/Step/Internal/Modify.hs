{-# language Safe #-}

module Step.Internal.Modify where

import Step.Internal.Dependencies

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM f = do
    x <- get
    x' <- lift (f x)
    put x'
