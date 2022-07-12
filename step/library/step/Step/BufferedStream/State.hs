module Step.BufferedStream.State where

import Step.Internal.Prelude

import qualified ListLike

import Step.BufferedStream.Base (BufferedStream)
import qualified Step.BufferedStream.Base as BufferedStream

import qualified Step.Buffer.Base as Buffer

import qualified Step.Buffer.State as Buffer.State

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial

-- | Determines whether there are any more
isEmpty :: (Monad m, ListLike text char) => StateT (BufferedStream m text) m Bool
isEmpty = do
    modifyM (BufferedStream.fillBuffer 1)
    get <&> BufferedStream.bufferIsEmpty

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached
fillBuffer :: (Monad m, ListLike text char) => Natural -> StateT (BufferedStream m text) m ()
fillBuffer n = modifyM (BufferedStream.fillBuffer n)

-- | Read one chunk of input; does nothing if the end of the stream has been reached
bufferMore :: (Monad m, ListLike text char) => StateT (BufferedStream m text) m ()
bufferMore = modifyM BufferedStream.bufferMore

-- | Force the entirety of the pending stream into the buffer
bufferAll :: (Monad m, ListLike text char) => StateT (BufferedStream m text) m ()
bufferAll = isEmpty >>= \case True -> return (); False -> bufferMore *> bufferAll

-- | Remove some text from the buffered stream, buffering more first if necessary, returning 'Nothing' if the end of the stream has been reached
takeChunk :: (Monad m, ListLike text char) => StateT (BufferedStream m text) m (Maybe (Nontrivial text))
takeChunk = do
    modifyM (BufferedStream.fillBuffer 1)
    zoom BufferedStream.bufferLens Buffer.State.takeChunk

-- | Remove some text where all characters satisfy the predicate, buffering more first if necessary, returning 'Nothing' if the stream does not begin with a character that satisfies the predicate
takeChunkWhile :: (Monad m, ListLike text char) => (char -> Bool) -> StateT (BufferedStream m text) m (Maybe (Nontrivial text))
takeChunkWhile ok =
    takeChunk >>= \case
        Nothing -> return Nothing
        Just x -> case Nontrivial.span ok x of
            Nontrivial.All -> return (Just x)
            Nontrivial.None -> putNontrivialChunk x $> Nothing
            Nontrivial.Split a b -> putNontrivialChunk b $> Just a

-- | Remove one character from the buffered stream, buffering more first if necessary, returning 'Nothing' if the end of the stream has been reached
takeChar :: (Monad m, ListLike text char) => StateT (BufferedStream m text) m (Maybe char)
takeChar = do
    modifyM (BufferedStream.fillBuffer 1)
    zoom BufferedStream.bufferLens Buffer.State.takeChar

-- todo: add an atomic version of takeTextNotAtomic

-- | Remove a particular expected prefix from the buffered stream, returning a 'Bool' indicating whether the operation succeeded
--
-- This operation is not atomic; it might take some characters and then fail.
--
takeTextNotAtomic :: (Monad m, ListLike text char, Eq text, Eq char) => text -> StateT (BufferedStream m text) m Bool
takeTextNotAtomic x = case Nontrivial.refine x of Nothing -> return True; Just y -> takeNontrivialTextNotAtomic y

-- | Variant of 'takeTextNotAtomic' that requires the text parameter to be non-empty
takeNontrivialTextNotAtomic :: (Monad m, ListLike text char, Eq text, Eq char) => Nontrivial text -> StateT (BufferedStream m text) m Bool
takeNontrivialTextNotAtomic c =
    isEmpty >>= \case
        True -> return False
        False -> zoom BufferedStream.bufferLens (Buffer.State.takeNontrivialString c) >>= \case
            Buffer.State.TakeStringFail -> return False
            Buffer.State.TakeStringSuccess -> return True
            Buffer.State.TakeStringPartial c' -> takeNontrivialTextNotAtomic c'

-- | Adds a chunk back to the left side of the buffer if the argument is non-empty
putChunk :: (Monad m, ListLike text char) => text -> StateT (BufferedStream m text) m ()
putChunk x = modify' (BufferedStream.putChunk x)

putNontrivialChunk :: (Monad m, ListLike text char) => Nontrivial text -> StateT (BufferedStream m text) m ()
putNontrivialChunk x = modify' (BufferedStream.putNontrivialChunk x)

takeBuffer :: (Monad m, Monoid text) => StateT (BufferedStream m text) m text
takeBuffer = do
    s <- get
    put s{ BufferedStream.buffer = Buffer.empty }
    return (Buffer.fold (BufferedStream.buffer s))
