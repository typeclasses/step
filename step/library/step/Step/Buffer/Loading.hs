{-# language FlexibleContexts, FlexibleInstances, DerivingVia #-}

module Step.Buffer.Loading
  (
    loadingCursor,
    bufferMore,
  )
  where

import Step.Internal.Prelude hiding (fold)

import qualified Step.Cursor as Cursor
import Step.Cursor (Stream, AdvanceResult (..), Cursor (Cursor), expandStateCursor)

import Step.RST (RST (..), contramapRST)

import qualified Step.Buffer.BufferState as BufferState
import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, unseen, uncommitted, newDoubleBuffer)
import Step.Buffer.BufferState (bufferStateCursor)

import Optics

loadingCursor :: forall xs x m s. ListLike xs x => Monad m =>
    Cursor xs x (Stream () s m xs x) (s, Buffer xs x) m
loadingCursor = Cursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    init :: (s, Buffer xs x) -> (s, DoubleBuffer xs x)
    init = over _2 newDoubleBuffer

    extract :: (s, DoubleBuffer xs x) -> (s, Buffer xs x)
    extract = over _2 (view uncommitted)

    input, bufferedInput, freshInput ::
        Stream (Stream () s m xs x) (s, DoubleBuffer xs x) m xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream (zoom (_2 % unseen) BufferState.takeChunk)
    freshInput = Cursor.Stream (bufferMore *> Cursor.next bufferedInput)

    commit, commitBuffered, commitFresh :: Positive Natural
        -> RST (Stream () s m xs x) (s, DoubleBuffer xs x) m AdvanceResult
    commit n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = zoom (_2 % uncommitted) (BufferState.dropN n)
    commitFresh n = bufferMore *> commitBuffered n

bufferMore :: Monad m =>
    RST (Stream () s m xs x) (s, DoubleBuffer xs x) m BufferResult
bufferMore =
    ask >>= \upstream ->
    zoom _1 (contramapRST (\_ -> ()) (Cursor.next upstream)) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (_2 % uncommitted % chunks) (:|> x)
            modifying (_2 % unseen % chunks) (:|> x)
            return BufferedMore
