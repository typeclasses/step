module Step.Error where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Functor.Identity

class ErrorContext e m | m -> e
  where
    getError :: m e

instance ErrorContext () Identity
  where
    getError = Identity ()

instance Applicative m => ErrorContext () (IdentityT m)
  where
    getError = IdentityT (pure ())

instance Applicative m => ErrorContext r (ReaderT r m)
  where
    getError = ReaderT pure
