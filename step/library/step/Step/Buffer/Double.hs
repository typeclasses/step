module Step.Buffer.Double
  (
    {- * Type -} DoubleBuffer (..),
    {- * Creation -} newDoubleBuffer,
    {- * Optics -} uncommittedLens, unseenLens,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Buffer (Buffer)
import qualified Step.Buffer.Buffer as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Result (BufferResult(..))

data DoubleBuffer xs x =
  DoubleBuffer
    { uncommitted :: Buffer xs x
    , unseen :: Buffer xs x
    }

newDoubleBuffer :: Buffer xs x -> DoubleBuffer xs x
newDoubleBuffer b = DoubleBuffer b b

uncommittedLens :: Lens (DoubleBuffer xs x) (DoubleBuffer xs x) (Buffer xs x) (Buffer xs x)
uncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

unseenLens :: Lens (DoubleBuffer xs x) (DoubleBuffer xs x) (Buffer xs x) (Buffer xs x)
unseenLens = lens unseen \x y -> x{ unseen = y }
