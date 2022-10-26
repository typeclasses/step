module Step.Error where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Void
import SupplyChain
import System.IO
import Data.Functor
import Control.Monad.Trans.Class
import Data.Function

class ErrorContext e m | m -> e
  where
    getError :: Job up m e

instance ErrorContext () (Const Void)
  where
    getError = pure ()

instance ErrorContext () Identity
  where
    getError = pure ()

instance ErrorContext () IO
  where
    getError = pure ()

instance ErrorContext e m => ErrorContext e (IdentityT m)
  where
    getError = alterAction (IdentityT @m) (getError @e @m)

instance (Monad m, ErrorContext e m) => ErrorContext (r, e) (ReaderT r m)
  where
    getError = perform ask >>= \r ->
        alterAction (ReaderT @r @m . const) (getError @e @m)
            <&> (r,)
