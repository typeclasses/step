{-# language FlexibleContexts, FlexibleInstances, DerivingVia #-}

module Step.Buffer.Loading (Loading (..)) where

import Step.Internal.Prelude hiding (fold)

import Step.Cursor (AdvanceResult (..), Cursory (..), Stream)
import qualified Step.Cursor as Cursor
import Step.Cursor (Stream, AdvanceResult (..), Cursory (..), Cursor (Cursor))

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.BufferState (BufferState (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, unseen, uncommitted)

newtype Loading xs x buffer m a =
    Loading (Stream m xs x -> BufferState xs x buffer m a)
    deriving (Functor, Applicative, Monad)
        via ReaderT (Stream m xs x) (BufferState xs x buffer m)

instance MonadTrans (Loading xs x buffer) where
    lift a = Loading \_ -> lift a

instance (Monad m, ListLike xs x) => Cursory (Loading xs x Buffer m) where
    type CursoryText (Loading xs x Buffer m) = xs
    type CursoryChar (Loading xs x Buffer m) = x
    type CursoryContext (Loading xs x Buffer m) = Loading xs x DoubleBuffer m
    curse = loadingCursor

loadingCursor :: forall xs x m. ListLike xs x => Monad m =>
    Cursor xs x (Loading xs x Buffer m) (Loading xs x DoubleBuffer m)
loadingCursor = Cursor{ Cursor.run, Cursor.input, Cursor.commit }
  where
    run (Loading f) = Loading \upstream -> Cursor.run curse (f upstream)

    input = Cursor.streamChoice bufferedInput freshInput
      where
        bufferedInput =
            Cursor.input c & Cursor.rebaseStream \a ->
                Loading \_ -> a
        freshInput = Cursor.stream (bufferMore *> Cursor.next bufferedInput)

    commit =
        \n -> commitBuffered n >>= \case
            r@AdvanceSuccess -> return r
            YouCanNotAdvance n' -> commitFresh n'
      where
        commitBuffered n = Loading \_ -> Cursor.commit c n
        commitFresh n = bufferMore *> commitBuffered n

    c = curse @(BufferState xs x Buffer m)

bufferMore :: Monad m => Loading xs x DoubleBuffer m BufferResult
bufferMore = Loading \upstream ->
    BufferState $ lift (Cursor.next upstream) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (uncommitted % chunks) (:|> x)
            modifying (unseen % chunks) (:|> x)
            return BufferedMore
