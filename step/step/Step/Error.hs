module Step.Error where

import Data.Functor.Identity

class ErrorContext e m | m -> e
  where
    getError :: m e

instance ErrorContext () Identity
  where
    getError = Identity ()
