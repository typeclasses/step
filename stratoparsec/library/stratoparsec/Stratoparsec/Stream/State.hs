module Stratoparsec.Stream.State where

import Optics

import ListLike (ListLike)
import qualified ListLike

import Stratoparsec.Stream (Stream)

import qualified Stratoparsec.Stream as Stream
import qualified Stratoparsec.Buffer.State as Buffer.State

import Stratoparsec.Util.Modify

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
