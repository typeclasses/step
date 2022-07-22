{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, NamedFieldPuns, TypeFamilies #-}

module Step.Input.BufferedStream
  (
    {- * The type -} BufferedStream (..), BufferResult (..), curse,
    {- * Conversion with Stream -} fromStream,
    {- * Buffer querying -} bufferIsEmpty, bufferedHeadChar,
    {- * Buffer manipulation -} bufferUnconsChunk, bufferUnconsChar,
    {- * Taking by chunk -} takeChunk,
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

import Step.Input.Buffering (Buffering (..))

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
        modifying (bufferSessionLens % BufferSession.uncommittedLens) (<> Buffer.singleton x)
        modifying (bufferSessionLens % BufferSession.unseenLens) (<> Buffer.singleton x)
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

instance (Monad m, ListLike text char) => Buffering (StateT (BufferedStream m text char) m) where

    fillBuffer1 = do
        ie <- get <&> Buffer.isEmpty . buffer
        when ie bufferMore

    bufferMore = use pendingLens >>= \p ->
        lift (Stream.next p) >>= traverse_ \x ->
            modifying bufferLens (<> Buffer.singleton x)

---

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

bufferIsEmpty :: BufferedStream m text char -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

bufferUnconsChunk :: BufferedStream m text char -> Maybe (Nontrivial text char, BufferedStream m text char)
bufferUnconsChunk s = case Buffer.unconsChunk (buffer s) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, s{ buffer = b' })

bufferUnconsChar :: ListLike text char => BufferedStream m text char -> Maybe (char, BufferedStream m text char)
bufferUnconsChar s = do
    (c, b') <- Buffer.unconsChar (buffer s)
    Just (c, s{ buffer = b' })

bufferedHeadChar :: ListLike text char => BufferedStream m text char -> Maybe char
bufferedHeadChar = Buffer.headChar . buffer

-- | Remove some text from the buffered stream, buffering more first if necessary, returning 'Nothing' if the end of the stream has been reached
takeChunk :: (ListLike text char, Monad m) => StateT (BufferedStream m text char) m (Maybe (Nontrivial text char))
takeChunk = do
    fillBuffer1
    zoom bufferLens Buffer.takeChunk
