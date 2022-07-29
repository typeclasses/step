{-# language FlexibleContexts, FlexibleInstances, DerivingVia #-}

module Step.Buffer.Loading (loadingCursor) where

import Step.Internal.Prelude hiding (fold)

import qualified Step.Cursor as Cursor
import Step.Cursor (Stream, AdvanceResult (..), Cursor (Cursor))

import Step.RST (RST (..))

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, unseen, uncommitted)
import Step.Buffer.BufferState (bufferStateCursor)

loadingCursor :: forall xs x m. ListLike xs x => Monad m =>
    Cursor xs x (Stream m xs x) (DoubleBuffer xs x) (Buffer xs x) m
loadingCursor = Cursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    c :: Cursor xs x r (DoubleBuffer xs x) (Buffer xs x) m
    c = bufferStateCursor

    init :: RST (Stream m xs x) (Buffer xs x) m (DoubleBuffer xs x)
    init = Cursor.init c

    extract :: RST (Stream m xs x) (DoubleBuffer xs x) m (Buffer xs x)
    extract = Cursor.extract c

    input, bufferedInput, freshInput ::
        Stream (RST (Stream m xs x) (DoubleBuffer xs x) m) xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.input c
    freshInput = Cursor.stream (bufferMore *> Cursor.next bufferedInput)

    commit, commitBuffered, commitFresh :: Positive Natural
        -> RST (Stream m xs x) (DoubleBuffer xs x) m AdvanceResult
    commit n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = Cursor.commit c n
    commitFresh n = bufferMore *> commitBuffered n

    bufferMore :: RST (Stream m xs x) (DoubleBuffer xs x) m BufferResult
    bufferMore =
        ask >>= \upstream ->
        lift (Cursor.next upstream) >>= \case
            Nothing -> return NothingToBuffer
            Just x -> do
                modifying (uncommitted % chunks) (:|> x)
                modifying (unseen % chunks) (:|> x)
                return BufferedMore
