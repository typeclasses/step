{-# language FlexibleContexts, DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Streaming
  (
    BufferedStreamSession (..),
    curseBufferedStream,
    runBufferedStreamSession,
    bufferedStreamSessionInput,
    bufferedStreamSessionCommit,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Result (BufferResult(..))

import Step.Buffer.Double (DoubleBuffer (DoubleBuffer), unseenLens, uncommittedLens)

import Step.Buffer.Session (BufferSession (BufferSession), runBufferSession, bufferSessionInput, bufferSessionCommit)
import qualified Step.Buffer.Session as BufferSession

newtype BufferedStreamSession xs x m a =
    BufferedStreamSession (Stream m xs x -> BufferSession xs x m a)
    deriving (Functor, Applicative, Monad)
        via ReaderT (Stream m xs x) (BufferSession xs x m)

curseBufferedStream :: Monad m => ListLike xs x =>
    Stream m xs x -> Cursor xs x (StateT (Buffer xs x) m) (BufferedStreamSession xs x m)
curseBufferedStream upstream =
  Cursor
    { run = runBufferedStreamSession upstream
    , commit = bufferedStreamSessionCommit
    , input = bufferedStreamSessionInput
    }

runBufferedStreamSession :: Monad m => ListLike xs x =>
    Stream m xs x -> BufferedStreamSession xs x m a -> StateT (Buffer xs x) m a
runBufferedStreamSession upstream (BufferedStreamSession f) =
    runBufferSession (f upstream)

bufferedStreamSessionInput :: Monad m => ListLike xs x =>
    Stream (BufferedStreamSession xs x m) xs x
bufferedStreamSessionInput = Cursor.streamChoice bufferedInput freshInput
  where
    bufferedInput =
        bufferSessionInput & Cursor.rebaseStream \a ->
            BufferedStreamSession \_ -> a
    freshInput = Cursor.stream (bufferMore *> Cursor.next bufferedInput)

bufferedStreamSessionCommit :: Monad m => ListLike xs x => Positive Natural -> BufferedStreamSession xs x m AdvanceResult
bufferedStreamSessionCommit =
    \n -> commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
  where
    commitBuffered n = BufferedStreamSession \_ -> bufferSessionCommit n
    commitFresh n = bufferMore *> commitBuffered n

bufferMore :: Monad m => BufferedStreamSession xs x m BufferResult
bufferMore = BufferedStreamSession \upstream ->
    BufferSession $ lift (Cursor.next upstream) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying uncommittedLens (Buffer.|> x)
            modifying unseenLens (Buffer.|> x)
            return BufferedMore
