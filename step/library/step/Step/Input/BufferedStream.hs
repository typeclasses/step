{-# language FlexibleContexts, FlexibleInstances #-}

module Step.Input.BufferedStream
  (
    BufferedStream (..), LoadingDoubleBufferState,
    curse, fromStream, bufferMore,
  )
  where

import Step.Internal.Prelude

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferState (BufferState, runBufferState)
import Step.Buffer.DoubleBuffer (DoubleBuffer (..), unseenLens, uncommittedLens)
import Step.Buffer.BufferResult (BufferResult (..))

import qualified Step.Buffer.BufferState as BufferState
import qualified Step.Buffer.DoubleBuffer as DoubleBuffer

import Step.Cursor (Cursor (..), AdvanceResult (..), shortfall, Stream)
import qualified Step.Cursor as Cursor

---

data BufferedStream m xs x =
  BufferedStream
    { buffer :: Buffer xs x
    , pending :: Stream m xs x
    }

data LoadingDoubleBufferState m xs x =
  LoadingDoubleBufferState
    { sessionPending :: Stream m xs x
    , bufferSession :: DoubleBuffer xs x
    }

sessionPendingLens :: Lens (LoadingDoubleBufferState m1 xs x) (LoadingDoubleBufferState m2 xs x) (Stream m1 xs x) (Stream m2 xs x)
sessionPendingLens = lens sessionPending \x y -> x{ sessionPending = y }

bufferSessionLens :: Lens (LoadingDoubleBufferState m xs x) (LoadingDoubleBufferState m xs x) (DoubleBuffer xs x) (DoubleBuffer xs x)
bufferSessionLens = lens bufferSession \x y -> x{ bufferSession = y }

sessionBufferMore :: Monad m => StateT (LoadingDoubleBufferState m xs x) m BufferResult
sessionBufferMore = use sessionPendingLens >>= \p -> lift (Cursor.next p) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> do
        modifying (bufferSessionLens % uncommittedLens % chunks) (:|> x)
        modifying (bufferSessionLens % unseenLens % chunks) (:|> x)
        return BufferedMore

curse :: forall m xs x. Monad m => ListLike xs x =>
    Cursor xs x (StateT (BufferedStream m xs x) m) (StateT (LoadingDoubleBufferState m xs x) m)
curse = Cursor{ run, commit, input }
  where
    run :: StateT (LoadingDoubleBufferState m xs x) m a -> StateT (BufferedStream m xs x) m a
    run a = do
        bs <- get
        (x, bss) <- lift (runStateT a (LoadingDoubleBufferState{ sessionPending = pending bs, bufferSession = DoubleBuffer.newDoubleBuffer (buffer bs) }))
        put BufferedStream{ buffer = DoubleBuffer.uncommitted (bufferSession bss), pending = sessionPending bss }
        return x

    input :: Stream (StateT (LoadingDoubleBufferState m xs x) m) xs x
    input = Cursor.stream $
        zoom (bufferSessionLens % DoubleBuffer.unseenLens) (runBufferState BufferState.takeChunk) >>= \case
            Just x -> return (Just x)
            Nothing -> sessionBufferMore >>= \case
                NothingToBuffer -> return Nothing
                BufferedMore -> zoom (bufferSessionLens % DoubleBuffer.unseenLens) (runBufferState BufferState.takeChunk)

    commit :: Positive Natural -> StateT (LoadingDoubleBufferState m xs x) m AdvanceResult
    commit n =
        zoom (bufferSessionLens % DoubleBuffer.uncommittedLens) (runBufferState (BufferState.dropN n)) >>= \case
            AdvanceSuccess -> return AdvanceSuccess
            YouCanNotAdvance{ shortfall = n' } -> sessionBufferMore >>= \case
                NothingToBuffer -> return YouCanNotAdvance{ shortfall = n' }
                BufferedMore -> zoom (bufferSessionLens % DoubleBuffer.uncommittedLens) (runBufferState (BufferState.dropN n))

bufferMore :: Monad m => StateT (BufferedStream m xs x) m ()
bufferMore = use pendingLens >>= \p ->
    lift (Cursor.next p) >>= traverse_ \x ->
        modifying (bufferLens % chunks) (:|> x)

bufferLens :: Lens (BufferedStream m xs x) (BufferedStream m xs x) (Buffer xs x) (Buffer xs x)
bufferLens = lens buffer \x y -> x{ buffer = y }

pendingLens :: Lens (BufferedStream m1 xs x) (BufferedStream m2 xs x) (Stream m1 xs x) (Stream m2 xs x)
pendingLens = lens pending \x y -> x{ pending = y }

fromStream :: Monad m => Stream m xs x -> BufferedStream m xs x
fromStream xs = BufferedStream{ buffer = [], pending = xs }
