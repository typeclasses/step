module Pushback
  (
    {- * Interface -} {- $interface -} Pushback (..),
            Step (..), PushbackStream (..), next, push,
    {- * Stack -} {- $stack -} Stack, StackPlus,
            substateStack,
    {- * Buffer -} {- $buffer -} Buffer, BufferPlus,
            substateBuffer,
    {- * Stack container -} {- $stackContainer -}
            StackContainer (..), list, sequence,
    {- * Stack effect -} {- $stackEffect -}
            StackEffect (..),
  )
  where

import Pushback.Interface
    (Pushback (..), Step (..), PushbackStream (..),
        next, push)
import Pushback.Stack
    (Stack, StackPlus)
import Pushback.Buffer
    (BufferPlus, Buffer)
import Pushback.StackEffect
    (StackEffect (StackEffect))
import Pushback.StackContainer
    (StackContainer (StackContainer),
        list, sequence)

import qualified Pushback.Buffer as Buffer
import qualified Pushback.Stack as Stack

import Control.Monad.State (MonadState)

import qualified Optics as O

{- $interface
See "Pushback.Interface" -}

{- $stack
See "Pushback.Stack" -}

{- $buffer
See "Pushback.Buffer" -}

{- $stackEffect
See "Pushback.StackEffect" -}

{- $stackContainer
See "Pushback.StackContainer" -}

{-| Buffers the upstream interface using monadic state to store
    pushed back items, with a 'O.Lens'' identifying what part
    of the state to use

See "Pushback.Buffer.Examples" for more examples. -}
substateBuffer :: MonadState state action =>
    O.Lens' state container
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> StackContainer container item
        -- ^ Push/pop operations for the container
        --   (see "Pushback.StackContainer.Examples")
    -> BufferPlus up action item
substateBuffer = Buffer.substate

{-| A stack that is stored entirely in memory, as one part (identified
    by a 'O.Lens'') of the state in a 'MonadState' context

For more examples, see "Pushback.Stack.Examples". -}
substateStack :: MonadState state action =>
    O.Lens' state container
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> StackContainer container item
        -- ^ Push/pop operations for the container
        --   (see "Pushback.StackContainer.Examples")
    -> StackPlus up action item
substateStack = Stack.substate
