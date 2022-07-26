{-# language DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Session
  (
    curse, drink,
    BufferCursor (..),

    {- * Optics -} newBufferCursor, uncommittedLens, unseenLens,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Result (BufferResult(..))

---

data BufferCursor xs x = BufferCursor
  { uncommitted :: Buffer xs x
  , unseen :: Buffer xs x
  }

newBufferCursor :: Buffer xs x -> BufferCursor xs x
newBufferCursor b = BufferCursor b b

uncommittedLens :: Lens (BufferCursor xs x) (BufferCursor xs x) (Buffer xs x) (Buffer xs x)
uncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

unseenLens :: Lens (BufferCursor xs x) (BufferCursor xs x) (Buffer xs x) (Buffer xs x)
unseenLens = lens unseen \x y -> x{ unseen = y }

---

drink :: Monad m => Stream m xs x -> StateT (BufferCursor xs x) m BufferResult
drink xs = lift (Cursor.next xs) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> do
        modifying uncommittedLens (Buffer.|> x)
        modifying unseenLens (Buffer.|> x)
        return BufferedMore

---

newtype BufferSession xs x m a =
    BufferSession (StateT (BufferCursor xs x) m a)
    deriving newtype (Functor, Applicative, Monad)

curse :: Monad m => ListLike xs x => Cursor xs x (StateT (Buffer xs x) m)
curse = Cursor{ run = runBufferSession, commit = bufferSessionCommit, input = bufferSessionInput }

runBufferSession :: Monad m => ListLike xs x => BufferSession xs x m a -> StateT (Buffer xs x) m a
runBufferSession (BufferSession a) = do
    b <- get
    (x, bs) <- lift (runStateT a (BufferCursor b b))
    put (uncommitted bs)
    return x

bufferSessionInput :: Monad m => ListLike xs x => Stream (BufferSession xs x m) xs x
bufferSessionInput = Cursor.stream $ BufferSession $ zoom unseenLens Buffer.takeChunk

bufferSessionCommit :: Monad m => ListLike xs x => Positive Natural -> BufferSession xs x m AdvanceResult
bufferSessionCommit n = BufferSession $ zoom uncommittedLens (Buffer.dropN n)
