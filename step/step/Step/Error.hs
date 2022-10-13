module Step.Error where

import Control.Applicative
import Control.Monad.Identity
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
