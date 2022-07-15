{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.BufferedStream.Base
  (
    {- * The type -} BufferedStream (..),
    {- * Optics -} bufferLens, pendingLens,
    {- * Constants -} empty,
    {- * Conversion with ListT -} toListT, fromListT,
    {- * Buffer querying -} bufferIsEmpty, isAllBuffered, bufferedHeadChar,
    {- * Buffer manipulation -} bufferUnconsChunk, bufferUnconsChar, putChunk, putNontrivialChunk,
    {- * Buffering -} fillBuffer1, bufferMore,
    {- * Taking by chunk -} takeChunk, takeChunkWhile,
  )
  where

import Step.Internal.Prelude

import qualified ListT

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer
import qualified Step.Buffer.State as Buffer.State

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.ListT as Nontrivial.ListT
import qualified Step.Nontrivial.List as Nontrivial

import qualified Step.Classes.Base as Class

import Step.TakeOrLeave (TakeOrLeave (..))

---

data BufferedStream m text =
  BufferedStream
    { buffer :: Buffer text
    , pending :: Maybe (ListT m (Nontrivial text))
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

instance Monad m => Class.Peek1 (StateT (BufferedStream m text) m) where
    type Text (StateT (BufferedStream m text) m) = text
    peekCharMaybe = modifyM fillBuffer1 *> (get <&> bufferedHeadChar)
    atEnd = modifyM fillBuffer1 *> (get <&> bufferIsEmpty)

instance Monad m => Class.Take1 (StateT (BufferedStream m text) m) where
    considerChar f = do
        modifyM fillBuffer1
        bs <- get
        case bufferUnconsChar bs of
            Nothing -> return Nothing
            Just (c, bs') -> Just <$> case f c of
                Leave r -> return (Leave r)
                Take r -> put bs' $> Take r

instance Monad m => Class.TakeAll (StateT (BufferedStream m text) m) where
    takeAll = bufferAll *> takeBuffer
      where
        bufferAll = isEmpty >>= \case
            True -> return ()
            False -> modifyM bufferMore *> bufferAll
        takeBuffer = do
            s <- get
            put s{ buffer = Buffer.empty }
            return (Buffer.fold (buffer s))

instance (Monad m, Eq text) => Class.SkipTextNonAtomic (StateT (BufferedStream m text) m) where
    skipTextNonAtomic x =
        case Nontrivial.refine x of
            Nothing -> return True
            Just y -> skipNontrivialTextNonAtomic y
      where
        skipNontrivialTextNonAtomic c =
            isEmpty >>= \case
                True -> return False
                False -> zoom bufferLens (Buffer.State.takeNontrivialString c) >>= \case
                    Buffer.State.TakeStringFail -> return False
                    Buffer.State.TakeStringSuccess -> return True
                    Buffer.State.TakeStringPartial c' -> skipNontrivialTextNonAtomic c'

---

bufferLens :: Lens' (BufferedStream m text) (Buffer text)
bufferLens = lens buffer \x y -> x{ buffer = y }

pendingLens :: Lens
    (BufferedStream m1 text)
    (BufferedStream m2 text)
    (Maybe (ListT m1 (Nontrivial text)))
    (Maybe (ListT m2 (Nontrivial text)))
pendingLens = lens pending \x y -> x{ pending = y }

empty :: BufferedStream m text
empty = BufferedStream Buffer.empty Nothing

isEmpty :: Monad m => StateT (BufferedStream m text) m Bool
isEmpty = do
    modifyM fillBuffer1
    get <&> bufferIsEmpty

isAllBuffered :: BufferedStream m text -> Bool
isAllBuffered = isNothing . pending

toListT :: Monad m => BufferedStream m text -> ListT m (Nontrivial text)
toListT x = Buffer.toListT (buffer x) <|> asum (pending x)

fromListT :: ListLike text char => Monad m => ListT m text -> BufferedStream m text
fromListT x = BufferedStream{ buffer = Buffer.empty, pending = Just (Nontrivial.ListT.filter x) }

bufferIsEmpty :: BufferedStream m text -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

bufferUnconsChunk :: BufferedStream m text -> Maybe (Nontrivial text, BufferedStream m text)
bufferUnconsChunk s = case Buffer.unconsChunk (buffer s) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, s{ buffer = b' })

bufferUnconsChar :: ListLike text char => BufferedStream m text -> Maybe (char, BufferedStream m text)
bufferUnconsChar s = do
    (c, b') <- Buffer.unconsChar (buffer s)
    Just (c, s{ buffer = b' })

bufferedHeadChar :: ListLike text char => BufferedStream m text -> Maybe char
bufferedHeadChar = Buffer.headChar . buffer

-- | Remove some text from the buffered stream, buffering more first if necessary, returning 'Nothing' if the end of the stream has been reached
takeChunk :: Monad m => StateT (BufferedStream m text) m (Maybe (Nontrivial text))
takeChunk = do
    modifyM fillBuffer1
    zoom bufferLens Buffer.State.takeChunk

-- | Remove some text where all characters satisfy the predicate, buffering more first if necessary, returning 'Nothing' if the stream does not begin with a character that satisfies the predicate
takeChunkWhile :: (Monad m, ListLike text char) => (char -> Bool) -> StateT (BufferedStream m text) m (Maybe (Nontrivial text))
takeChunkWhile ok =
    takeChunk >>= \case
        Nothing -> return Nothing
        Just x -> case Nontrivial.span ok x of
            Nontrivial.All -> return (Just x)
            Nontrivial.None -> modify' (putNontrivialChunk x) $> Nothing
            Nontrivial.Split a b -> modify' (putNontrivialChunk b) $> Just a

-- | Adds a chunk back to the left side of the buffer if the argument is non-empty
putChunk :: ListLike text char => text -> BufferedStream m text -> BufferedStream m text
putChunk x s = case Nontrivial.refine x of Nothing -> s; Just y -> putNontrivialChunk y s

-- | Adds a chunk back to the left side of the buffer
putNontrivialChunk :: Nontrivial text -> BufferedStream m text -> BufferedStream m text
putNontrivialChunk x s = s{ buffer = Buffer.singleton x <> buffer s }

fillBuffer1 :: Monad m => BufferedStream m text -> m (BufferedStream m text)
fillBuffer1 b = if Buffer.isEmpty (buffer b) then bufferMore b else return b

-- | Read one chunk of input; does nothing if the end of the stream has been reached
bufferMore :: Monad m =>
    BufferedStream m text -> m (BufferedStream m text)
bufferMore s = case pending s of
    Nothing -> return s -- If the end of the stream has been reached, do nothing
    Just p ->
        ListT.next p -- Perform the next step in the pending input stream
        >>= \case
            ListT.Nil -> -- If the stream is now empty, change its value to 'Nothing' to remember that we have reached the end
                return s{ pending = Nothing }
            ListT.Cons x xs -> -- We got a new chunk of input.
                return BufferedStream{
                    buffer = buffer s <> Buffer.singleton x, -- Add the chunk to the buffer if it is non-empty.
                    pending = Just xs -- Remove the chunk from the pending input stream.
                }
