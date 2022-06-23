module Stratoparsec.Stream.State where

import Mono (MonoFoldable)


import Stratoparsec.Stream (Stream)
import qualified Stratoparsec.Stream as Stream

import Stratoparsec.Util.Modify

-- | Determines whether there are any more
isEmpty :: (Monad m, MonoFoldable a) => StateT (Stream m a) m Bool
isEmpty = do
    s <- get
    s' <- lift (Stream.fillBuffer 1 s)
    put s'
    return (Stream.bufferIsEmpty s')

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, MonoFoldable a) => Natural -> StateT (Stream m a) m ()
fillBuffer n = modifyM (Stream.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
readChunk :: (Monad m, MonoFoldable chunk) => StateT (Stream m chunk) m ()
readChunk = modifyM Stream.readChunk
