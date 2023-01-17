module Pushback.Stack.Examples
  (
    containerState,
  )
  where

import Essentials
import Pushback.Interface
import Pushback.Stack.Type
import Pushback.Container

import Next (Step (..))
import Control.Monad.State (MonadState)

import qualified Optics as O
import qualified SupplyChain.Vendor as Vendor

containerState :: MonadState state action =>
    StackContainer container item
    -> O.Lens' state container
    -> StackPlus up action item
containerState StackContainer{ push, pop } containerLens =
    Vendor.action \case
        Next -> do
            container <- O.use containerLens
            case pop container of
                Nothing -> pure End
                Just (x, xs) -> do
                    O.assign' containerLens xs
                    pure (Item x)
        Push x -> do
            container <- O.use containerLens
            O.assign' containerLens (push x container)
