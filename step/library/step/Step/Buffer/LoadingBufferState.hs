{-# language FlexibleContexts, DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.LoadingBufferState where

import Step.Internal.Prelude hiding (fold)

import Step.Cursor (AdvanceResult (..), Cursory (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult(..))
import Step.Buffer.DoubleBuffer (DoubleBuffer (DoubleBuffer), unseenLens, uncommittedLens)
import Step.Buffer.DoubleBufferState (DoubleBufferState (..))
import Step.Buffer.LoadingDoubleBufferState (LoadingDoubleBufferState (..))
import Step.Buffer.BufferState (BufferState (..))

newtype LoadingBufferState xs x m a =
    LoadingBufferState (Stream m xs x -> BufferState xs x m a)
    deriving (Functor, Applicative, Monad)
        via ReaderT (Stream m xs x) (BufferState xs x m)

instance (Monad m, ListLike xs x) => Cursory (LoadingBufferState xs x m) where
    type CursoryText (LoadingBufferState xs x m) = xs
    type CursoryChar (LoadingBufferState xs x m) = x
    type CursoryContext (LoadingBufferState xs x m) = LoadingDoubleBufferState xs x m

    cursoryRun (LoadingDoubleBufferState f) =
        LoadingBufferState \upstream -> cursoryRun (f upstream)

    cursoryInput = Cursor.streamChoice bufferedInput freshInput
      where
        bufferedInput =
            cursoryInput @(BufferState xs x m) & Cursor.rebaseStream \a ->
                LoadingDoubleBufferState \_ -> a
        freshInput = Cursor.stream (bufferMore *> Cursor.next bufferedInput)

    cursoryCommit =
        \n -> commitBuffered n >>= \case
            r@AdvanceSuccess -> return r
            YouCanNotAdvance n' -> commitFresh n'
      where
        commitBuffered n = LoadingDoubleBufferState \_ -> cursoryCommit @(BufferState xs x m) n
        commitFresh n = bufferMore *> commitBuffered n

bufferMore :: Monad m => LoadingDoubleBufferState xs x m BufferResult
bufferMore = LoadingDoubleBufferState \upstream ->
    DoubleBufferState $ lift (Cursor.next upstream) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (uncommittedLens % chunks) (:|> x)
            modifying (unseenLens % chunks) (:|> x)
            return BufferedMore
