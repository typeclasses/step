module Step.Failure where

import Step.Internal.Prelude

class Monad m => Fallible m where
    type Error m :: Type
    failure :: m (Error m)

instance Fallible m => Fallible (ReaderT r m) where
    type Error (ReaderT r m) = Error m
    failure = lift failure
