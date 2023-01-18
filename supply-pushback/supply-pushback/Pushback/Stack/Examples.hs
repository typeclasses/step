module Pushback.Stack.Examples
  (
    {- * Examples -} state, substate, effect,
  )
  where

import Essentials
import Pushback.Interface.Type
import Pushback.Stack.Type
import Pushback.StackEffect.Type

import Control.Monad.State (MonadState)
import Pushback.StackContainer (StackContainer)

import qualified Pushback.StackContainer as StackContainer
import qualified Pushback.StackEffect as StackEffect
import qualified SupplyChain.Vendor as Vendor
import qualified Optics as O

effect :: Functor action =>
    StackEffect action item   -- ^ Effectful push/pop operations
    -> StackPlus up action item
effect StackEffect{ pop, push } =
    Vendor.action \case
        Next -> pop <&> \case{ Nothing -> End; Just x -> Item x }
        Push x -> push x

{-| A stack that is stored entirely in memory, as the
    state in a 'MonadState' context -}
state :: MonadState container action =>
    StackContainer container item
        -- ^ Push/pop operations for the container
        --   (see "Pushback.StackContainer.Examples")
    -> StackPlus up action item
state container = effect $ StackEffect.state $ container

{-| A stack that is stored entirely in memory, as one part (identified
    by a 'O.Lens'') of the state in a 'MonadState' context -}
substate :: MonadState state action =>
    O.Lens' state container
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> StackContainer container item
        -- ^ Push/pop operations for the container
        --   (see "Pushback.StackContainer.Examples")
    -> StackPlus up action item
substate containerLens container = state $
    StackContainer.zoom containerLens container
