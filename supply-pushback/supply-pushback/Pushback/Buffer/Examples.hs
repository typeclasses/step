module Pushback.Buffer.Examples
  (
    {- * Examples -} state, substate, effect,
  )
  where

import Essentials
import Pushback.Buffer.Type
import Pushback.StackEffect.Type
import Pushback.Interface.Type

import Control.Monad.State (MonadState)
import Pushback.StackContainer (StackContainer)

import qualified Next
import qualified SupplyChain.Job as Job
import qualified SupplyChain
import qualified Pushback.StackContainer as StackContainer
import qualified Pushback.StackEffect as StackEffect
import qualified Optics as O

{-| Buffers the upstream interface with the addition of
    a stack action to store pushed-back items -}
effect ::
    StackEffect action item
        -- ^ Effectful push/pop operations
        --   (see "Pushback.StackEffect.Examples")
    -> BufferPlus up action item
effect StackEffect{ push, pop } =
    SupplyChain.loop' \case
        Next -> Job.perform pop >>= \case
            Nothing -> Job.order Next.next
            Just x -> pure (Item x)
        Push x -> Job.perform (push x)

{-| Buffers the upstream interface by using monadic state
    to store items that have been pushed back -}
state :: MonadState container action =>
    StackContainer container item
        -- ^ Push/pop operations for the container
        --   (see "Pushback.StackContainer.Examples")
    -> BufferPlus up action item
state container = effect $ StackEffect.state container

{-| Buffers the upstream interface using monadic state to store
    pushed back items, with a 'O.Lens'' identifying what part
    of the state to use -}
substate :: MonadState state action =>
    O.Lens' state container
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> StackContainer container item
        -- ^ Push/pop operations for the container
        --   (see "Pushback.StackContainer.Examples")
    -> BufferPlus up action item
substate containerLens container =
    effect $ StackEffect.state $
        StackContainer.zoom containerLens container
