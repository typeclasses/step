module Step.Buffer.Session
  (
    curse, drink,
    BufferCursor (..),

    {- * Optics -} newBufferSession, uncommittedLens, unseenLens,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (Cursor), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.Result (BufferResult(..))

data BufferCursor xs x = BufferCursor
  { uncommitted :: Buffer xs x
  , unseen :: Buffer xs x
  }

newBufferSession :: Buffer xs x -> BufferCursor xs x
newBufferSession b = BufferCursor b b

uncommittedLens :: Lens (BufferCursor xs x) (BufferCursor xs x) (Buffer xs x) (Buffer xs x)
uncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

unseenLens :: Lens (BufferCursor xs x) (BufferCursor xs x) (Buffer xs x) (Buffer xs x)
unseenLens = lens unseen \x y -> x{ unseen = y }

curse :: Monad m => ListLike xs x => Cursor xs x (StateT (Buffer xs x) m)
curse = Cursor{ Cursor.run, Cursor.commit, Cursor.input }

run :: Monad m => ListLike xs x => StateT (BufferCursor xs x) m a -> StateT (Buffer xs x) m a
run a = do
    b <- get
    (x, bs) <- lift (runStateT a (BufferCursor b b))
    put (uncommitted bs)
    return x

input :: Monad m => ListLike xs x => Stream (StateT (BufferCursor xs x) m) xs x
input = Cursor.stream (zoom unseenLens Buffer.takeChunk)

commit :: Monad m => ListLike xs x => Positive Natural -> StateT (BufferCursor xs x) m AdvanceResult
commit n = zoom uncommittedLens (Buffer.dropN n)

drink :: Monad m => Stream m xs x -> StateT (BufferCursor xs x) m BufferResult
drink xs = lift (Cursor.next xs) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> do
        modifying uncommittedLens (Buffer.|> x)
        modifying unseenLens (Buffer.|> x)
        return BufferedMore
