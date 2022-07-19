{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.Input.BufferedStream
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

import Step.Input.Buffer (Buffer)
import qualified Step.Input.Buffer as Buffer

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.ListT as Nontrivial.ListT
import qualified Step.Nontrivial.List as Nontrivial

import qualified Step.Classes.Base as Class

import Step.TakeOrLeave (TakeOrLeave (..))

import Step.Advancement (AdvanceResult, Progressive (..))
import qualified Step.Advancement as Advance

---

data BufferedStream m text char =
  BufferedStream
    { buffer :: Buffer text char
    , pending :: Maybe (ListT m (Nontrivial text char))
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

instance (Monad m, ListLike text char) => Progressive (StateT (BufferedStream m text char) m) where
    advance n =
        zoom bufferLens (advance n) >>= \case
            Advance.Success -> return Advance.Success
            Advance.InsufficientInput n' -> use pendingLens >>= \case
                Nothing -> return (Advance.InsufficientInput n')
                Just p -> do
                    lift (ListT.next p) >>= \case
                        ListT.Nil -> assign pendingLens Nothing $> Advance.InsufficientInput n'
                        ListT.Cons x xs -> do
                            modifying bufferLens (<> Buffer.singleton x)
                            assign pendingLens (Just xs)
                            advance n'

instance (Monad m, ListLike text char) => Class.Char1 (StateT (BufferedStream m text char) m) where
    type Text (StateT (BufferedStream m text char) m) = text
    type Char (StateT (BufferedStream m text char) m) = char
    peekCharMaybe = Class.fillBuffer1 *> (get <&> bufferedHeadChar)
    atEnd = Class.fillBuffer1 *> (get <&> bufferIsEmpty)
    considerChar (Class.Consideration1 f) = do
        Class.fillBuffer1
        bs <- get
        case bufferUnconsChar bs of
            Nothing -> return Nothing
            Just (c, bs') -> Just <$> case f c of
                Leave r -> return (Leave r)
                Take r -> put bs' $> Take r

instance (Monad m, ListLike text char) => Class.TakeAll (StateT (BufferedStream m text char) m) where
    takeAll = bufferAll *> takeBuffer
      where
        bufferAll = isEmpty >>= \case
            True -> return ()
            False -> Class.bufferMore *> bufferAll
        takeBuffer = do
            s <- get
            put s{ buffer = Buffer.empty }
            return (Buffer.fold (buffer s))

instance (Monad m, Eq text, Eq char, ListLike text char) => Class.SkipTextNonAtomic (StateT (BufferedStream m text char) m) where
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

instance (Monad m, ListLike text char) => Class.FillBuffer1 (StateT (BufferedStream m text char) m) where
    fillBuffer1 = do
        ie <- get <&> Buffer.isEmpty . buffer
        when ie Class.bufferMore

instance (Monad m, ListLike text char) => Class.BufferMore (StateT (BufferedStream m text char) m) where
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

bufferLens :: Lens' (BufferedStream m text char) (Buffer text char)
bufferLens = lens buffer \x y -> x{ buffer = y }

pendingLens :: Lens
    (BufferedStream m1 text char)
    (BufferedStream m2 text char)
    (Maybe (ListT m1 (Nontrivial text char)))
    (Maybe (ListT m2 (Nontrivial text char)))
pendingLens = lens pending \x y -> x{ pending = y }

empty :: BufferedStream m text char
empty = BufferedStream Buffer.empty Nothing

isEmpty :: (Monad m, ListLike text char) => StateT (BufferedStream m text char) m Bool
isEmpty = do
    Class.fillBuffer1
    get <&> bufferIsEmpty

isAllBuffered :: BufferedStream m text char -> Bool
isAllBuffered = isNothing . pending

toListT :: Monad m => BufferedStream m text char -> ListT m (Nontrivial text char)
toListT x = Buffer.toListT (buffer x) <|> asum (pending x)

fromListT :: ListLike text char => Monad m => ListT m text -> BufferedStream m text char
fromListT x = BufferedStream{ buffer = Buffer.empty, pending = Just (Nontrivial.ListT.filter x) }

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
    Class.fillBuffer1
    zoom bufferLens Buffer.takeChunk

considerChunk :: Monad m => ListLike text char => (Nontrivial text char -> (Natural, a)) -> StateT (BufferedStream m text char) m (Maybe a)
considerChunk f = zoom bufferLens (Buffer.considerChunk f)
