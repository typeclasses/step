module Step.Buffer.Cursor
  (
    {- * Type -} BufferCursor (..),
    {- * Creation -} newBufferCursor,
    {- * Optics -} uncommittedLens, unseenLens,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Result (BufferResult(..))

data BufferCursor xs x = BufferCursor
  { uncommitted :: Buffer xs x
  , unseen :: Buffer xs x
  }

newBufferCursor :: Buffer xs x -> BufferCursor xs x
newBufferCursor b = BufferCursor b b

uncommittedLens :: Lens (BufferCursor xs x) (BufferCursor xs x) (Buffer xs x) (Buffer xs x)
uncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

unseenLens :: Lens (BufferCursor xs x) (BufferCursor xs x) (Buffer xs x) (Buffer xs x)
unseenLens = lens unseen \x y -> x{ unseen = y }
