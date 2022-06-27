module Step.Util.Modify where

import Step.Util.Prelude

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM f = do
    x <- get
    x' <- lift (f x)
    put x'
