module Step.Cursor.State where

import Step.Internal.Prelude

import Step.Cursor.Base (Cursor)
import qualified Step.Cursor.Base as Cursor
import qualified Step.Cursor.Tentative as Cursor.Tentative

import qualified Step.Tentative.State as Tentative.State

import qualified Step.BufferedStream.State as BufferedStream.State

import qualified Step.Buffer.Base as Buffer

import qualified ListLike

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike text char) => Natural -> StateT (Cursor m text) m ()
fillBuffer n = modifyM (Cursor.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
bufferMore :: (Monad m, ListLike text char) => StateT (Cursor m text) m ()
bufferMore = modifyM Cursor.bufferMore

takeChar :: (Monad m, ListLike text char) => StateT (Cursor m text) m (Maybe char)
takeChar = fillBuffer 1 *> takeBufferedChar

peekCharMaybe :: Monad m => ListLike text char => StateT (Cursor m text) m (Maybe char)
peekCharMaybe = fillBuffer 1 *> peekBufferedChar

takeBufferedChar :: Monad m => ListLike text char => StateT (Cursor m text) m (Maybe char)
takeBufferedChar = do
    s <- get
    case Cursor.bufferUnconsChar s of
        Nothing -> return Nothing
        Just (c, s') -> put s' $> Just c

peekBufferedChar :: Monad m => ListLike text char => StateT (Cursor m text) m (Maybe char)
peekBufferedChar = get <&> Cursor.bufferHeadChar

takeCharIf :: Monad m => ListLike text char => (char -> Bool) -> StateT (Cursor m text) m (Maybe char)
takeCharIf f = Tentative.State.ifJust (\case Just x | f x -> Just x; _ -> Nothing) Cursor.Tentative.takeChar

takeCharJust :: Monad m => ListLike text char => (char -> Maybe r) -> StateT (Cursor m text) m (Maybe r)
takeCharJust f = Tentative.State.ifJust (>>= f) Cursor.Tentative.takeChar

takeTextNotAtomic :: (Monad m, ListLike text char, Eq text, Eq char) => text -> StateT (Cursor m text) m Bool
takeTextNotAtomic x = do
    y <- zoom Cursor.bufferedStreamLens (BufferedStream.State.takeTextNotAtomic x)
    modifying Cursor.positionLens (+ fromIntegral (ListLike.length x))
    return y

bufferAll :: (Monad m, ListLike text char) => StateT (Cursor m text) m ()
bufferAll = modifyM Cursor.bufferAll

takeAll :: (Monad m, ListLike text char) => StateT (Cursor m text) m text
takeAll = bufferAll *> takeBuffer

takeBuffer :: ListLike text char => Monoid text => Monad m => StateT (Cursor m text) m text
takeBuffer = do
    x <- zoom Cursor.bufferedStreamLens BufferedStream.State.takeBuffer
    modifying Cursor.positionLens (+ fromIntegral (ListLike.length x))
    return x

atEnd :: ListLike text char => Monad m => StateT (Cursor m text) m Bool
atEnd = fillBuffer 1 *> (get <&> Cursor.bufferIsEmpty)

isAllBuffered :: Monad m => StateT (Cursor m text) m Bool
isAllBuffered = get <&> Cursor.isAllBuffered
