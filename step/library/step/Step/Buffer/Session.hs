module Step.Buffer.Session
  (
    curse, drink,
    BufferSession (..),

    {- * Optics -} newBufferSession, uncommittedLens, unseenLens,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Nontrivial (Nontrivial)

import Step.Input.AdvanceResult (AdvanceResult)

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Input.Cursor (Session (Session))
import qualified Step.Input.Cursor as Session

import qualified Step.Input.Stream as Stream
import Step.Input.Stream (Stream (..))

import Step.Buffer.Result (BufferResult(..))

data BufferSession xs x = BufferSession
  { uncommitted :: Buffer xs x
  , unseen :: Buffer xs x
  }

newBufferSession :: Buffer xs x -> BufferSession xs x
newBufferSession b = BufferSession b b

uncommittedLens :: Lens (BufferSession xs x) (BufferSession xs x) (Buffer xs x) (Buffer xs x)
uncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

unseenLens :: Lens (BufferSession xs x) (BufferSession xs x) (Buffer xs x) (Buffer xs x)
unseenLens = lens unseen \x y -> x{ unseen = y }

curse :: Monad m => ListLike xs x => Session xs x (StateT (Buffer xs x) m)
curse = Session{ Session.run, Session.commit, Session.input }

run :: Monad m => ListLike xs x => StateT (BufferSession xs x) m a -> StateT (Buffer xs x) m a
run a = do
    b <- get
    (x, bs) <- lift (runStateT a (BufferSession b b))
    put (uncommitted bs)
    return x

input :: Monad m => ListLike xs x => Stream (StateT (BufferSession xs x) m) (Nontrivial xs x)
input = Stream{ next = zoom unseenLens Buffer.takeChunk }

commit :: Monad m => ListLike xs x => Positive Natural -> StateT (BufferSession xs x) m AdvanceResult
commit n = zoom uncommittedLens (Buffer.dropN n)

drink :: Monad m => Stream m (Nontrivial xs x) -> StateT (BufferSession xs x) m BufferResult
drink xs = lift (Stream.next xs) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> do
        modifying uncommittedLens (Buffer.|> x)
        modifying unseenLens (Buffer.|> x)
        return BufferedMore
