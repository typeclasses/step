module Step.Buffer.Session
  (
    curseBuffer, curseBufferedStream,
    BufferSession (..),
    drink,

    {- * Optics -} newBufferSession, uncommittedLens, unseenLens,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Nontrivial (Nontrivial)

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult, shortfall)

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Input.Cursor (Session (Session))
import qualified Step.Input.Cursor as Session

import qualified Step.Input.Stream as Stream
import Step.Input.Stream (Stream (..))

import Step.Buffer.Result

data BufferSession text char = BufferSession
  { uncommitted :: Buffer text char
  , unseen :: Buffer text char
  }

newBufferSession :: Buffer text char -> BufferSession text char
newBufferSession b = BufferSession b b

uncommittedLens :: Lens
  (BufferSession text char)
  (BufferSession text char)
  (Buffer text char)
  (Buffer text char)
uncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

unseenLens :: Lens
  (BufferSession text char)
  (BufferSession text char)
  (Buffer text char)
  (Buffer text char)
unseenLens = lens unseen \x y -> x{ unseen = y }

drink :: Monad m => Stream m (Nontrivial text char) -> StateT (BufferSession text char) m BufferResult
drink xs = lift (Stream.next xs) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> do
        modifying uncommittedLens (Buffer.|> x)
        modifying unseenLens (Buffer.|> x)
        return BufferedMore

curseBuffer :: forall m text char. Monad m => ListLike text char =>
    Session text char (StateT (Buffer text char) m)
curseBuffer = Session{ Session.run, Session.commit, Session.input }
  where
    run :: Monad m => ListLike text char => StateT (BufferSession text char) m a -> StateT (Buffer text char) m a
    run a = do
        b <- get
        (x, bs) <- lift (runStateT a (newBufferSession b))
        put (uncommitted bs)
        return x

    input :: Monad m => ListLike text char => Stream (StateT (BufferSession text char) m) (Nontrivial text char)
    input = Stream{ next = zoom unseenLens Buffer.takeChunk }

    commit :: Monad m => ListLike text char => Positive Natural -> StateT (BufferSession text char) m AdvanceResult
    commit n = zoom uncommittedLens (Buffer.dropN n)

curseBufferedStream :: forall m text char. Monad m => ListLike text char =>
    Stream m (Nontrivial text char) -> Session text char (StateT (Buffer text char) m)
curseBufferedStream upstream = Session{ Session.run, Session.commit, Session.input }
  where
    run :: StateT (BufferSession text char) m a -> StateT (Buffer text char) m a
    run a = do
        b <- get
        (x, bs) <- lift (runStateT a (newBufferSession b))
        put (uncommitted bs)
        return x

    input :: Stream (StateT (BufferSession text char) m) (Nontrivial text char)
    input = Stream
        { next =
              zoom unseenLens Buffer.takeChunk >>= \case
                  Just x -> return (Just x)
                  Nothing -> bufferMore >>= \case
                      NothingToBuffer -> return Nothing
                      BufferedMore -> zoom unseenLens Buffer.takeChunk
        }

    commit :: Positive Natural -> StateT (BufferSession text char) m AdvanceResult
    commit n =
        zoom uncommittedLens (Buffer.dropN n) >>= \case
            Advance.Success -> return Advance.Success
            Advance.InsufficientInput{ shortfall = n' } -> bufferMore >>= \case
                NothingToBuffer -> return Advance.InsufficientInput{ shortfall = n' }
                BufferedMore -> zoom uncommittedLens (Buffer.dropN n)

    bufferMore :: StateT (BufferSession text char) m BufferResult
    bufferMore = lift (Stream.next upstream) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (uncommittedLens) (Buffer.|> x)
            modifying (unseenLens) (Buffer.|> x)
            return BufferedMore
