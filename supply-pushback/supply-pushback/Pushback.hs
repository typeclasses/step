module Pushback
  (
    {- * Interface -} {- $interface -} Pushback (..),
            Step (..), PushbackStream (..), next, push,
    {- * Stack -} {- $stack -} Stack, StackPlus,
    {- * Buffer -} {- $buffer -} Buffer, BufferPlus,
    {- * Stack container -} {- $stackContainer -}
            StackContainer (..), list, sequence,
    {- * Stack effect -} {- $stackEffect -} StackEffect (..),
  )
  where

import Pushback.Interface (Pushback (..), Step (..), PushbackStream (..), next, push)
import Pushback.Stack (Stack, StackPlus)
import Pushback.Buffer (BufferPlus, Buffer)
import Pushback.StackEffect (StackEffect (StackEffect))
import Pushback.StackContainer (StackContainer (StackContainer), list, sequence)

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
