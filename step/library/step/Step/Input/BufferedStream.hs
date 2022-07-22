{-# language FlexibleContexts, FlexibleInstances #-}

module Step.Input.BufferedStream
  (
    BufferedStream (..), BufferResult (..),
    curse, fromStream, bufferMore,
  )
  where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Buffer.Session (BufferSession (..))
import qualified Step.Buffer.Session as BufferSession

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

import Step.Input.Cursor (Session (..))

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult, shortfall)

import Step.Input.Stream (Stream)
import qualified Step.Input.Stream as Stream

---

data BufferedStream m text char =
  BufferedStream
    { buffer :: Buffer text char
    , pending :: Stream m (Nontrivial text char)
    }

data BufferedStreamSession m text char =
  BufferedStreamSession
    { sessionPending :: Stream m (Nontrivial text char)
    , bufferSession :: BufferSession text char
    }

sessionPendingLens :: Lens
  (BufferedStreamSession m1 text char)
  (BufferedStreamSession m2 text char)
  (Stream m1 (Nontrivial text char))
  (Stream m2 (Nontrivial text char))
sessionPendingLens = lens sessionPending \x y -> x{ sessionPending = y }

bufferSessionLens :: Lens
  (BufferedStreamSession m text char)
  (BufferedStreamSession m text char)
  (BufferSession text char)
  (BufferSession text char)
bufferSessionLens = lens bufferSession \x y -> x{ bufferSession = y }

data BufferResult = BufferedMore | NothingToBuffer

sessionBufferMore :: Monad m => StateT (BufferedStreamSession m text char) m BufferResult
sessionBufferMore = use sessionPendingLens >>= \p -> lift (Stream.next p) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> do
        modifying (bufferSessionLens % BufferSession.uncommittedLens) (Buffer.|> x)
        modifying (bufferSessionLens % BufferSession.unseenLens) (Buffer.|> x)
        return BufferedMore

curse :: forall m text char. Monad m => ListLike text char => Session text char (StateT (BufferedStream m text char) m)
curse = Session{ run, commit, next }
  where
    run :: StateT (BufferedStreamSession m text char) m a -> StateT (BufferedStream m text char) m a
    run a = do
        bs <- get
        (x, bss) <- lift (runStateT a (BufferedStreamSession{ sessionPending = pending bs, bufferSession = BufferSession.newBufferSession (buffer bs) }))
        put BufferedStream{ buffer = BufferSession.uncommitted (bufferSession bss), pending = sessionPending bss }
        return x

    next :: StateT (BufferedStreamSession m text char) m (Maybe (Nontrivial text char))
    next =
        zoom (bufferSessionLens % BufferSession.unseenLens) Buffer.takeChunk >>= \case
            Just x -> return (Just x)
            Nothing -> sessionBufferMore >>= \case
                NothingToBuffer -> return Nothing
                BufferedMore -> zoom (bufferSessionLens % BufferSession.unseenLens) Buffer.takeChunk

    commit :: Positive Natural -> StateT (BufferedStreamSession m text char) m AdvanceResult
    commit n =
        zoom (bufferSessionLens % BufferSession.uncommittedLens) (Buffer.dropN n) >>= \case
            Advance.Success -> return Advance.Success
            Advance.InsufficientInput{ shortfall = n' } -> sessionBufferMore >>= \case
                NothingToBuffer -> return Advance.InsufficientInput{ shortfall = n' }
                BufferedMore -> zoom (bufferSessionLens % BufferSession.uncommittedLens) (Buffer.dropN n)

bufferMore :: Monad m => StateT (BufferedStream m text char) m ()
bufferMore = use pendingLens >>= \p ->
    lift (Stream.next p) >>= traverse_ \x ->
        modifying bufferLens (Buffer.|> x)

bufferLens :: Lens
  (BufferedStream m text char)
  (BufferedStream m text char)
  (Buffer text char)
  (Buffer text char)
bufferLens = lens buffer \x y -> x{ buffer = y }

pendingLens :: Lens
  (BufferedStream m1 text char)
  (BufferedStream m2 text char)
  (Stream m1 (Nontrivial text char))
  (Stream m2 (Nontrivial text char))
pendingLens = lens pending \x y -> x{ pending = y }

fromStream :: ListLike text char => Monad m => Stream m text -> BufferedStream m text char
fromStream xs = BufferedStream{ buffer = Buffer.empty, pending = Stream.mapMaybe Nontrivial.refine xs }
