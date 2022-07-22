module Step.Buffer.Session
  (
    curse,
    BufferSession (..),

    {- * Optics -} newBufferSession, uncommittedLens, unseenLens,
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Nontrivial.Base (Nontrivial)

import Step.Input.AdvanceResult (AdvanceResult)

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Input.Cursor (Session (Session))
import qualified Step.Input.Cursor as Session

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

curse :: Monad m => ListLike text char => Session text char (StateT (Buffer text char) m)
curse = Session{ Session.run, Session.commit, Session.next }

run :: Monad m => ListLike text char => StateT (BufferSession text char) m a -> StateT (Buffer text char) m a
run a = do
    b <- get
    (x, bs) <- lift (runStateT a (BufferSession b b))
    put (uncommitted bs)
    return x

next :: Monad m => ListLike text char => StateT (BufferSession text char) m (Maybe (Nontrivial text char))
next = zoom unseenLens Buffer.takeChunk

commit :: Monad m => ListLike text char => Positive Natural -> StateT (BufferSession text char) m AdvanceResult
commit n = zoom uncommittedLens (Buffer.dropN n)
