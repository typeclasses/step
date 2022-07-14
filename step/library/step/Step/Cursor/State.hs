module Step.Cursor.State where

import Step.Internal.Prelude

import Step.Cursor.Base (Cursor)
import qualified Step.Cursor.Base as Cursor

import qualified Step.BufferedStream.State as BufferedStream.State

import qualified Step.Buffer.Base as Buffer

import qualified ListLike

import Step.TakeOrLeave (TakeOrLeave)

fillBuffer1 :: Monad m => StateT (Cursor m text) m ()
fillBuffer1 = modifyM Cursor.fillBuffer1

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
bufferMore :: Monad m => StateT (Cursor m text) m ()
bufferMore = modifyM Cursor.bufferMore

takeChar :: (Monad m, ListLike text char) => StateT (Cursor m text) m (Maybe char)
takeChar = fillBuffer1 *> takeBufferedChar

peekCharMaybe :: Monad m => ListLike text char => StateT (Cursor m text) m (Maybe char)
peekCharMaybe = fillBuffer1 *> peekBufferedChar

takeBufferedChar :: Monad m => ListLike text char => StateT (Cursor m text) m (Maybe char)
takeBufferedChar = do
    s <- get
    case Cursor.bufferUnconsChar s of
        Nothing -> return Nothing
        Just (c, s') -> put s' $> Just c

peekBufferedChar :: Monad m => ListLike text char => StateT (Cursor m text) m (Maybe char)
peekBufferedChar = get <&> Cursor.bufferHeadChar

considerChar :: Monad m => ListLike text char => (char -> TakeOrLeave b a) -> StateT (Cursor m text) m (Maybe (TakeOrLeave b a))
considerChar f = StateT (Cursor.considerUnconsChar f)

takeTextNotAtomic :: (Monad m, ListLike text char, Eq text, Eq char) => text -> StateT (Cursor m text) m Bool
takeTextNotAtomic x = do
    y <- zoom Cursor.bufferedStreamLens (BufferedStream.State.takeTextNotAtomic x)
    modifying Cursor.positionLens (+ fromIntegral (ListLike.length x))
    return y

bufferAll :: Monad m => StateT (Cursor m text) m ()
bufferAll = modifyM Cursor.bufferAll

takeAll :: (Monad m, ListLike text char) => StateT (Cursor m text) m text
takeAll = bufferAll *> takeBuffer

takeBuffer :: ListLike text char => Monoid text => Monad m => StateT (Cursor m text) m text
takeBuffer = do
    x <- zoom Cursor.bufferedStreamLens BufferedStream.State.takeBuffer
    modifying Cursor.positionLens (+ fromIntegral (ListLike.length x))
    return x

atEnd :: Monad m => ListLike text char => StateT (Cursor m text) m Bool
atEnd = fillBuffer1 *> (get <&> Cursor.bufferIsEmpty)

isAllBuffered :: Monad m => StateT (Cursor m text) m Bool
isAllBuffered = get <&> Cursor.isAllBuffered
