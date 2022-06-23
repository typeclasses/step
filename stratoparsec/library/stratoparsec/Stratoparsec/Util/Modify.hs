module Stratoparsec.Util.Modify where

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM f = do
    x <- get
    x' <- lift (f x)
    put x'
