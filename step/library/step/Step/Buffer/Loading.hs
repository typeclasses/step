{-# language FlexibleContexts, FlexibleInstances, DerivingVia #-}

module Step.Buffer.Loading
  (
    loadingCursor,
  )
  where

import Step.Internal.Prelude hiding (fold)

import qualified Step.Cursor as Cursor
import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor))

import Step.RST (RST (..))

import qualified Step.Buffer.BufferState as BufferState
import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.BufferState (bufferStateCursor)

import qualified Optics

import Step.Nontrivial (Nontrivial)

loadingCursor :: forall s xs x m. ListLike xs x => Monad m =>
    Lens' s (Buffer xs x)
    -> ReadWriteCursor xs x (Stream () s m xs x) s m
loadingCursor bufferLens = ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init :: RST (Stream () s m xs x) s m (Buffer xs x)
    init = use bufferLens

    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream (zoom Cursor.ephemeralStateLens BufferState.takeChunk)
    freshInput = Cursor.Stream (bufferMore *> Cursor.next bufferedInput)

    commit n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = zoom (Cursor.committedStateLens % bufferLens) (BufferState.dropN n)
    commitFresh n = bufferMore *> commitBuffered n

    bufferMore = next >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (Cursor.committedStateLens % bufferLens % chunks) (:|> x)
            modifying (Cursor.ephemeralStateLens % chunks) (:|> x)
            return BufferedMore

    next = ask >>= \upstream ->
        zoom Cursor.committedStateLens $
            contramap (\_ -> ()) (Cursor.next upstream)
