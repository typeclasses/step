{-# language FlexibleContexts, FlexibleInstances, DerivingVia #-}

module Step.Buffer.Loading
  (
    loadingCursor,
  )
  where

import Step.Internal.Prelude hiding (fold)

import qualified Step.Cursor as Cursor
import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor), CursorState)

import Step.RST (RST (..))

import qualified Step.Buffer.BufferState as BufferState
import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.BufferState (bufferStateCursor)

import qualified Optics

import Step.Nontrivial (Nontrivial)

loadingCursor :: forall s xs x m. Monad m =>
     Lens' s (Buffer xs x) -> ReadWriteCursor xs x (Stream () s m xs x) s m
loadingCursor bufferLens = ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init :: RST r s m (Buffer xs x)
    init = use bufferLens

    input, bufferedInput, freshInput :: Stream (Stream () s m xs x) (CursorState (Buffer xs x) s) m xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream (zoom Cursor.ephemeralStateLens BufferState.takeChunk)
    freshInput = Cursor.Stream (bufferMore *> Cursor.next bufferedInput)

    commit, commitBuffered, commitFresh :: Positive Natural -> RST (Stream () s m xs x) (CursorState (Buffer xs x) s) m AdvanceResult
    commit n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = zoom (Cursor.committedStateLens % bufferLens) (BufferState.dropN n)
    commitFresh n = bufferMore *> commitBuffered n

    bufferMore :: RST (Stream () s m xs x) (CursorState (Buffer xs x) s) m BufferResult
    bufferMore = next >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (Cursor.committedStateLens % bufferLens % chunks) (:|> x)
            modifying (Cursor.ephemeralStateLens % chunks) (:|> x)
            return BufferedMore

    next :: RST (Stream () s m xs x) (CursorState ephemeral s) m (Maybe (Nontrivial xs x))
    next = ask >>= \upstream ->
        zoom Cursor.committedStateLens $
            contramap (\_ -> ()) (Cursor.next upstream)
