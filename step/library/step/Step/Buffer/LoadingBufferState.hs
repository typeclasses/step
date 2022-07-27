{-# language FlexibleInstances, DerivingVia #-}

module Step.Buffer.LoadingBufferState (LoadingBufferState (..)) where

import Step.Internal.Prelude hiding (fold)

import Step.Cursor (AdvanceResult (..), Cursory (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.BufferState (BufferState (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, unseen, uncommitted)

newtype LoadingBufferState xs x buffer m a =
    LoadingBufferState (Stream m xs x -> BufferState xs x buffer m a)
    deriving (Functor, Applicative, Monad)
        via ReaderT (Stream m xs x) (BufferState xs x buffer m)

instance MonadTrans (LoadingBufferState xs x buffer) where
    lift a = LoadingBufferState \_ -> lift a

instance (Monad m, ListLike xs x) => Cursory (LoadingBufferState xs x Buffer m) where
    type CursoryText (LoadingBufferState xs x Buffer m) = xs
    type CursoryChar (LoadingBufferState xs x Buffer m) = x
    type CursoryContext (LoadingBufferState xs x Buffer m) = LoadingBufferState xs x DoubleBuffer m

    cursoryRun (LoadingBufferState f) =
        LoadingBufferState \upstream -> cursoryRun (f upstream)

    cursoryInput = Cursor.streamChoice bufferedInput freshInput
      where
        bufferedInput =
            cursoryInput @(BufferState xs x Buffer m) & Cursor.rebaseStream \a ->
                LoadingBufferState \_ -> a
        freshInput = Cursor.stream (bufferMore *> Cursor.next bufferedInput)

    cursoryCommit =
        \n -> commitBuffered n >>= \case
            r@AdvanceSuccess -> return r
            YouCanNotAdvance n' -> commitFresh n'
      where
        commitBuffered n = LoadingBufferState \_ -> cursoryCommit @(BufferState xs x Buffer m) n
        commitFresh n = bufferMore *> commitBuffered n

bufferMore :: Monad m => LoadingBufferState xs x DoubleBuffer m BufferResult
bufferMore = LoadingBufferState \upstream ->
    BufferState $ lift (Cursor.next upstream) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (uncommitted % chunks) (:|> x)
            modifying (unseen % chunks) (:|> x)
            return BufferedMore
