module Step.Internal.Modify where

import BasePrelude
import State
import MonadTrans

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM f = do
    x <- get
    x' <- lift (f x)
    put x'
