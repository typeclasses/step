module Pushback.StackEffect.Examples
  (
    {- * Examples -} state, substate,
  )
  where

import Essentials
import Pushback.StackEffect.Type

import Pushback.StackContainer (StackContainer)
import Control.Monad.State (MonadState)

import qualified Control.Monad.State as State
import qualified Pushback.StackContainer as StackContainer
import qualified Optics as O

{-| A stack that is stored within a monadic state context -}
state :: MonadState container action =>
    StackContainer container item
        -- ^ Push/pop operations for the stack data structure
        --   (see "Pushback.StackContainer.Examples")
    -> StackEffect action item
state container = StackEffect{ push, pop }
  where
    push x = State.modify' (StackContainer.push container x)
    pop = do
          xs <- State.get
          case StackContainer.pop container xs of
              Nothing -> pure Nothing
              Just (x, remainder) ->
                  State.put remainder $> Just x

substate :: MonadState state action =>
    O.Lens' state container
        -- ^ Identifies where to store the stack
        --   within a larger state context
    -> StackContainer container item
        -- ^ Push/pop operations for the stack data structure
        --   (see "Pushback.StackContainer.Examples")
    -> StackEffect action item
substate o container = state (StackContainer.zoom o container)
