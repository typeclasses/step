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
fillBuffer :: (Monad m, ListLike chunk char) => Natural -> StateT (Cursor m chunk) m ()
fillBuffer n = modifyM (Cursor.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
readChunk :: (Monad m, ListLike chunk char) => StateT (Cursor m chunk) m ()
readChunk = modifyM Cursor.readChunk

takeChar :: (Monad m, ListLike chunk char) => StateT (Cursor m chunk) m (Maybe char)
takeChar = fillBuffer 1 *> takeBufferedChar

takeBufferedChar :: Monad m => ListLike chunk char => StateT (Cursor m chunk) m (Maybe char)
takeBufferedChar = do
    s <- get
    case Cursor.bufferUnconsChar s of
        Nothing -> return Nothing
        Just (c, s') -> do
            put s'
            return (Just c)

takeCharIf :: Monad m => ListLike chunk char => (char -> Bool) -> StateT (Cursor m chunk) m (Maybe char)
takeCharIf f = Tentative.State.ifJust (\case Just x | f x -> Just x; _ -> Nothing) Cursor.Tentative.takeChar

takeText :: (Monad m, ListLike chunk char, Eq chunk, Eq char) => chunk -> StateT (Cursor m chunk) m Bool
takeText x = do
    y <- zoom Cursor.bufferedStreamLens (BufferedStream.State.takeText x)
    modifying Cursor.positionLens (+ fromIntegral (ListLike.length x))
    return y

bufferAll :: (Monad m, ListLike chunk char) => StateT (Cursor m chunk) m ()
bufferAll = modifyM Cursor.bufferAll

takeAll :: (Monad m, ListLike chunk char) => StateT (Cursor m chunk) m chunk
takeAll = do
    bufferAll
    takeBuffer

takeBuffer :: Monoid chunk => Monad m => StateT (Cursor m chunk) m chunk
takeBuffer = do
    b <- use Cursor.bufferLens
    assign Cursor.bufferLens Buffer.empty
    modifying Cursor.positionLens (+ fromIntegral (Buffer.size b))
    return (Buffer.fold b)

atEnd :: ListLike chunk char => Monad m => StateT (Cursor m chunk) m Bool
atEnd = do
    fillBuffer 1
    get <&> Cursor.bufferIsEmpty

--     zoom ParseState.futureLens Stream.State.bufferAll
--     xs <- Buffer.chunks <$> use (ParseState.futureLens % Stream.bufferLens)
--     assign ParseState.futureLens Stream.empty
--     zoom ParseState.pastLens $ traverse_ (modify' . Past.record) xs
--     return (Right (ListLike.fold xs))
