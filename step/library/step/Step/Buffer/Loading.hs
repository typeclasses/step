{-# language FlexibleContexts, FlexibleInstances, DerivingVia #-}

module Step.Buffer.Loading (Loading (..), loadingCursor) where

import Step.Internal.Prelude hiding (fold)

import qualified Step.Cursor as Cursor
import Step.Cursor (Stream, AdvanceResult (..), Cursor (Cursor))

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, unseen, uncommitted)
import Step.Buffer.BufferState (bufferStateCursor)

newtype Loading xs x buffer m a =
    Loading (Stream m xs x -> StateT (buffer xs x) m a)
    deriving (Functor, Applicative, Monad)
        via ReaderT (Stream m xs x) (StateT (buffer xs x) m)

instance MonadTrans (Loading xs x buffer) where
    lift a = Loading \_ -> lift a

loadingCursor :: forall xs x m. ListLike xs x => Monad m =>
    Cursor xs x (Loading xs x Buffer m) (Loading xs x DoubleBuffer m)
loadingCursor = Cursor{ Cursor.run, Cursor.input, Cursor.commit }
  where
    run :: Loading xs x DoubleBuffer m a -> Loading xs x Buffer m a
    run (Loading f) = Loading \upstream -> Cursor.run c (f upstream)

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

    c = bufferStateCursor

bufferMore :: Monad m => Loading xs x DoubleBuffer m BufferResult
bufferMore = Loading \upstream ->
    lift (Cursor.next upstream) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (uncommitted % chunks) (:|> x)
            modifying (unseen % chunks) (:|> x)
            return BufferedMore
