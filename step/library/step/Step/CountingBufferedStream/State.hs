module Step.CountingBufferedStream.State where

import Step.Internal.Prelude

import Step.CountingBufferedStream.Base (CountingBufferedStream)
import qualified Step.CountingBufferedStream.Base as CountingBufferedStream

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike chunk char) => Natural -> StateT (CountingBufferedStream m chunk) m ()
fillBuffer n = modifyM (CountingBufferedStream.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
readChunk :: (Monad m, ListLike chunk char) => StateT (CountingBufferedStream m chunk) m ()
readChunk = modifyM CountingBufferedStream.readChunk

takeChar :: (Monad m, ListLike chunk char) => StateT (CountingBufferedStream m chunk) m (Maybe char)
takeChar = fillBuffer 1 *> takeBufferedChar

takeBufferedChar :: Monad m => ListLike chunk char => StateT (CountingBufferedStream m chunk) m (Maybe char)
takeBufferedChar = do
    s <- get
    case CountingBufferedStream.bufferUnconsChar s of
        Nothing -> return Nothing
        Just (c, s') -> do
            put s'
            return (Just c)
