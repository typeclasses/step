module Step.Stream.State where

import Step.Internal.Prelude

import qualified ListLike

import Step.Stream.Base (Stream)

import qualified Step.Stream.Base as Stream
import qualified Step.Buffer.State as Buffer.State

-- | Determines whether there are any more
isEmpty :: (Monad m, ListLike chunk char) => StateT (Stream m chunk) m Bool
isEmpty = do
    modifyM (Stream.fillBuffer 1)
    get <&> Stream.bufferIsEmpty

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike chunk char) => Natural -> StateT (Stream m chunk) m ()
fillBuffer n = modifyM (Stream.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
readChunk :: (Monad m, ListLike chunk char) => StateT (Stream m chunk) m ()
readChunk = modifyM Stream.readChunk

bufferAll :: (Monad m, ListLike chunk char) => StateT (Stream m chunk) m ()
bufferAll = isEmpty >>= \case True -> return (); False -> readChunk *> bufferAll

takeChunk :: (Monad m, ListLike chunk char) => StateT (Stream m chunk) m (Maybe chunk)
takeChunk = do
    modifyM (Stream.fillBuffer 1)
    zoom Stream.bufferLens Buffer.State.takeChunk

takeChar :: (Monad m, ListLike chunk char) => StateT (Stream m chunk) m (Maybe char)
takeChar = do
    modifyM (Stream.fillBuffer 1)
    zoom Stream.bufferLens Buffer.State.takeChar

takeString :: (Monad m, ListLike chunk char, Eq chunk, Eq char) => chunk -> StateT (Stream m chunk) m Bool
takeString c = if ListLike.null c then return True else
    isEmpty >>= \case
        True -> return False
        False -> zoom Stream.bufferLens (Buffer.State.takeString c) >>= \case
            Buffer.State.TakeStringFail -> return False
            Buffer.State.TakeStringSuccess -> return True
            Buffer.State.TakeStringPartial c' -> takeString c'

putChunk :: (Monad m, ListLike chunk char) => chunk -> StateT (Stream m chunk) m ()
putChunk x = unless (ListLike.null x) $ modify' (Stream.putChunk x)
