module Step.BufferedStream.State where

import Step.Internal.Prelude

import qualified ListLike

import Step.BufferedStream.Base (BufferedStream)
import qualified Step.BufferedStream.Base as BufferedStream

import qualified Step.Buffer.State as Buffer.State

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

-- | Determines whether there are any more
isEmpty :: (Monad m, ListLike chunk char) => StateT (BufferedStream m chunk) m Bool
isEmpty = do
    modifyM (BufferedStream.fillBuffer 1)
    get <&> BufferedStream.bufferIsEmpty

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike chunk char) => Natural -> StateT (BufferedStream m chunk) m ()
fillBuffer n = modifyM (BufferedStream.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
bufferMore :: (Monad m, ListLike chunk char) => StateT (BufferedStream m chunk) m ()
bufferMore = modifyM BufferedStream.bufferMore

bufferAll :: (Monad m, ListLike chunk char) => StateT (BufferedStream m chunk) m ()
bufferAll = isEmpty >>= \case True -> return (); False -> bufferMore *> bufferAll

takeChunk :: (Monad m, ListLike chunk char) => StateT (BufferedStream m chunk) m (Maybe (Nontrivial chunk))
takeChunk = do
    modifyM (BufferedStream.fillBuffer 1)
    zoom BufferedStream.bufferLens Buffer.State.takeChunk

takeChar :: (Monad m, ListLike chunk char) => StateT (BufferedStream m chunk) m (Maybe char)
takeChar = do
    modifyM (BufferedStream.fillBuffer 1)
    zoom BufferedStream.bufferLens Buffer.State.takeChar

takeText :: (Monad m, ListLike chunk char, Eq chunk, Eq char) => chunk -> StateT (BufferedStream m chunk) m Bool
takeText x = case Nontrivial.refine x of Nothing -> return True; Just y -> takeNontrivialText y

takeNontrivialText :: (Monad m, ListLike chunk char, Eq chunk, Eq char) => Nontrivial chunk -> StateT (BufferedStream m chunk) m Bool
takeNontrivialText c =
    isEmpty >>= \case
        True -> return False
        False -> zoom BufferedStream.bufferLens (Buffer.State.takeNontrivialString c) >>= \case
            Buffer.State.TakeStringFail -> return False
            Buffer.State.TakeStringSuccess -> return True
            Buffer.State.TakeStringPartial c' -> takeNontrivialText c'

putChunk :: (Monad m, ListLike chunk char) => chunk -> StateT (BufferedStream m chunk) m ()
putChunk x = unless (ListLike.null x) $ modify' (BufferedStream.putChunk x)
