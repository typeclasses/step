{-# language FlexibleContexts, FlexibleInstances, DerivingVia #-}

module Step.Buffer.Loading
  (
    loadingCursor,
  )
  where

import Step.Internal.Prelude hiding (fold)

import qualified Step.Cursor as Cursor
import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor))

import Step.RST (RST (..), contramapRST)

import qualified Step.Buffer.BufferState as BufferState
import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.BufferState (bufferStateCursor)

import qualified Optics

import Step.Nontrivial (Nontrivial)

loadingCursor :: forall xs x m s. ListLike xs x => Monad m =>
    Lens' s (Buffer xs x)
    -> ReadWriteCursor xs x (Stream () s m xs x) s m
loadingCursor bufferLens = ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    unseen :: Lens' (Buffer xs x, s) (Buffer xs x)
    unseen = Optics._1

    uncommitted :: Lens' (Buffer xs x, s) (Buffer xs x)
    uncommitted = Optics._2 % bufferLens

    init :: s -> Buffer xs x
    init = view bufferLens

    extract :: Buffer xs x -> s -> s
    extract _ = id

    input, bufferedInput, freshInput ::
        Stream (Stream () s m xs x) (Buffer xs x, s) m xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream (zoom unseen BufferState.takeChunk)
    freshInput = Cursor.Stream (bufferMore *> Cursor.next bufferedInput)

    commit, commitBuffered, commitFresh :: Positive Natural
        -> RST (Stream () s m xs x) (Buffer xs x, s) m AdvanceResult
    commit n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = zoom uncommitted (BufferState.dropN n)
    commitFresh n = bufferMore *> commitBuffered n

    bufferMore :: RST (Stream () s m xs x) (Buffer xs x, s) m BufferResult
    bufferMore = next >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (uncommitted % chunks) (:|> x)
            modifying (unseen % chunks) (:|> x)
            return BufferedMore

    next :: RST (Stream () s m xs x) (Buffer xs x, s) m (Maybe (Nontrivial xs x))
    next = ask >>= \upstream ->
        zoom Optics._2 $
            contramapRST (\_ -> ()) (Cursor.next upstream)
