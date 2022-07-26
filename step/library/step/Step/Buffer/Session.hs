{-# language DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Session
  (
    BufferSession (..),
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

import Step.Buffer.Result (BufferResult(..))

import Step.Buffer.Double (DoubleBuffer (DoubleBuffer), unseenLens, uncommittedLens)
import qualified Step.Buffer.Double as DoubleBuffer

newtype BufferSession xs x m a =
    BufferSession (StateT (DoubleBuffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad)

curseBuffer :: Monad m => ListLike xs x => Cursor xs x (StateT (Buffer xs x) m) (BufferSession xs x m)
curseBuffer =
   Cursor
    { run = runBufferSession
    , commit = bufferSessionCommit
    , input = bufferSessionInput
    }

runBufferSession :: Monad m => ListLike xs x => BufferSession xs x m a -> StateT (Buffer xs x) m a
runBufferSession (BufferSession a) = do
    b <- get
    (x, bs) <- lift (runStateT a (DoubleBuffer b b))
    put (view uncommittedLens bs)
    return x

bufferSessionInput :: Monad m => ListLike xs x => Stream (BufferSession xs x m) xs x
bufferSessionInput = Cursor.stream $ BufferSession $ zoom unseenLens Buffer.takeChunk

bufferSessionCommit :: Monad m => ListLike xs x => Positive Natural -> BufferSession xs x m AdvanceResult
bufferSessionCommit n = BufferSession $ zoom uncommittedLens (Buffer.dropN n)
