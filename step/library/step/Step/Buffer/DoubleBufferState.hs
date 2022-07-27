{-# language DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.DoubleBufferState
  (
    DoubleBufferState (..),
    curseBuffer,
    runBufferSession,
    bufferSessionInput,
    bufferSessionCommit,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Buffer (Buffer)
import qualified Step.Buffer.Buffer as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.BufferResult (BufferResult(..))

import Step.Buffer.DoubleBuffer (DoubleBuffer (DoubleBuffer), unseenLens, uncommittedLens)
import qualified Step.Buffer.DoubleBuffer as DoubleBuffer

import qualified Step.Buffer.BufferState as BufferState
import Step.Buffer.BufferState (runBufferState)

newtype DoubleBufferState xs x m a =
    DoubleBufferState (StateT (DoubleBuffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad)


-- todo: move stuff below into BufferState

curseBuffer :: Monad m => ListLike xs x => Cursor xs x (StateT (Buffer xs x) m) (DoubleBufferState xs x m)
curseBuffer =
   Cursor
    { run = runBufferSession
    , commit = bufferSessionCommit
    , input = bufferSessionInput
    }

runBufferSession :: Monad m => ListLike xs x => DoubleBufferState xs x m a -> StateT (Buffer xs x) m a
runBufferSession (DoubleBufferState a) = do
    b <- get
    (x, bs) <- lift (runStateT a (DoubleBuffer b b))
    put (view uncommittedLens bs)
    return x

bufferSessionInput :: Monad m => ListLike xs x => Stream (DoubleBufferState xs x m) xs x
bufferSessionInput = Cursor.stream $ DoubleBufferState $ zoom unseenLens (runBufferState BufferState.takeChunk)

bufferSessionCommit :: Monad m => ListLike xs x => Positive Natural -> DoubleBufferState xs x m AdvanceResult
bufferSessionCommit n = DoubleBufferState $ zoom uncommittedLens (runBufferState (BufferState.dropN n))
