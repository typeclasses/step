{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.BufferedStream.Base
  (
    {- * The type -} BufferedStream (..),
    {- * Constants -} empty,
    {- * Conversion with ListT -} toListT, fromListT,
    {- * Buffer querying -} bufferIsEmpty, isAllBuffered, bufferedHeadChar,
    {- * Buffer manipulation -} bufferUnconsChunk, bufferUnconsChar,
    {- * Taking by chunk -} takeChunk, considerChunk,
  )
  where

import Step.Internal.Prelude

import qualified ListT

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

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
    peekCharMaybe = Class.fillBuffer1 *> (get <&> bufferedHeadChar)
    atEnd = Class.fillBuffer1 *> (get <&> bufferIsEmpty)

instance Monad m => Class.Take1 (StateT (BufferedStream m text) m) where
    considerChar f = do
        Class.fillBuffer1
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
            False -> Class.bufferMore *> bufferAll
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
                False -> zoom bufferLens (Buffer.takeNontrivialString c) >>= \case
                    Buffer.TakeStringFail -> return False
                    Buffer.TakeStringSuccess -> return True
                    Buffer.TakeStringPartial c' -> skipNontrivialTextNonAtomic c'

instance Monad m => Class.FillBuffer1 (StateT (BufferedStream m text) m) where
    fillBuffer1 = do
        ie <- get <&> Buffer.isEmpty . buffer
        when ie Class.bufferMore

instance Monad m => Class.BufferMore (StateT (BufferedStream m text) m) where
    bufferMore = (get <&> pending) >>= \case

        -- If the end of the stream has been reached, do nothing
        Nothing -> return ()

        Just p ->

            -- Perform the next step in the pending input stream
            lift (ListT.next p)

            >>= \case

                -- If the stream is now empty, change its value to 'Nothing' to remember that we have reached the end
                ListT.Nil -> assign pendingLens Nothing

                -- We got a new chunk of input.
                ListT.Cons x xs -> do

                    -- Add the chunk to the buffer
                    modifying bufferLens (<> Buffer.singleton x)

                    -- Remove the chunk from the pending input stream
                    assign pendingLens (Just xs)

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
    Class.fillBuffer1
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
    Class.fillBuffer1
    zoom bufferLens Buffer.takeChunk

considerChunk :: Monad m => ListLike text char => (Nontrivial text -> (Natural, a)) -> StateT (BufferedStream m text) m (Maybe a)
considerChunk f = zoom bufferLens (Buffer.considerChunk f)
