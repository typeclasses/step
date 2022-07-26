{-# language FlexibleContexts, FlexibleInstances #-}

module Step.Input.BufferedStream
  (
    BufferedStream (..),
    curse, fromStream, bufferMore,
  )
  where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Buffer.Cursor (BufferCursor (..))
import qualified Step.Buffer.Cursor as BufferCursor

import Step.Cursor (Cursor (..), AdvanceResult (..), shortfall, Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Result (BufferResult (..))

---

data BufferedStream m xs x =
  BufferedStream
    { buffer :: Buffer xs x
    , pending :: Stream m xs x
    }

data BufferedStreamSession m xs x =
  BufferedStreamSession
    { sessionPending :: Stream m xs x
    , bufferSession :: BufferCursor xs x
    }

sessionPendingLens :: Lens (BufferedStreamSession m1 xs x) (BufferedStreamSession m2 xs x) (Stream m1 xs x) (Stream m2 xs x)
sessionPendingLens = lens sessionPending \x y -> x{ sessionPending = y }

bufferSessionLens :: Lens (BufferedStreamSession m xs x) (BufferedStreamSession m xs x) (BufferCursor xs x) (BufferCursor xs x)
bufferSessionLens = lens bufferSession \x y -> x{ bufferSession = y }

sessionBufferMore :: Monad m => StateT (BufferedStreamSession m xs x) m BufferResult
sessionBufferMore = use sessionPendingLens >>= \p -> lift (Cursor.next p) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> do
        modifying (bufferSessionLens % BufferCursor.uncommittedLens) (Buffer.|> x)
        modifying (bufferSessionLens % BufferCursor.unseenLens) (Buffer.|> x)
        return BufferedMore

curse :: forall m xs x. Monad m => ListLike xs x => Cursor xs x (StateT (BufferedStream m xs x) m)
curse = Cursor{ run, commit, input }
  where
    run :: StateT (BufferedStreamSession m xs x) m a -> StateT (BufferedStream m xs x) m a
    run a = do
        bs <- get
        (x, bss) <- lift (runStateT a (BufferedStreamSession{ sessionPending = pending bs, bufferSession = BufferCursor.newBufferCursor (buffer bs) }))
        put BufferedStream{ buffer = BufferCursor.uncommitted (bufferSession bss), pending = sessionPending bss }
        return x

    input :: Stream (StateT (BufferedStreamSession m xs x) m) xs x
    input = Cursor.stream $
        zoom (bufferSessionLens % BufferCursor.unseenLens) Buffer.takeChunk >>= \case
            Just x -> return (Just x)
            Nothing -> sessionBufferMore >>= \case
                NothingToBuffer -> return Nothing
                BufferedMore -> zoom (bufferSessionLens % BufferCursor.unseenLens) Buffer.takeChunk

    commit :: Positive Natural -> StateT (BufferedStreamSession m xs x) m AdvanceResult
    commit n =
        zoom (bufferSessionLens % BufferCursor.uncommittedLens) (Buffer.dropN n) >>= \case
            AdvanceSuccess -> return AdvanceSuccess
            YouCanNotAdvance{ shortfall = n' } -> sessionBufferMore >>= \case
                NothingToBuffer -> return YouCanNotAdvance{ shortfall = n' }
                BufferedMore -> zoom (bufferSessionLens % BufferCursor.uncommittedLens) (Buffer.dropN n)

bufferMore :: Monad m => StateT (BufferedStream m xs x) m ()
bufferMore = use pendingLens >>= \p ->
    lift (Cursor.next p) >>= traverse_ \x ->
        modifying bufferLens (Buffer.|> x)

bufferLens :: Lens (BufferedStream m xs x) (BufferedStream m xs x) (Buffer xs x) (Buffer xs x)
bufferLens = lens buffer \x y -> x{ buffer = y }

pendingLens :: Lens (BufferedStream m1 xs x) (BufferedStream m2 xs x) (Stream m1 xs x) (Stream m2 xs x)
pendingLens = lens pending \x y -> x{ pending = y }

fromStream :: Monad m => Stream m xs x -> BufferedStream m xs x
fromStream xs = BufferedStream{ buffer = Buffer.empty, pending = xs }
