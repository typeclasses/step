{-# language GeneralizedNewtypeDeriving #-}

module Step.Buffer.BufferState (BufferState (..), takeChunk, dropN) where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import Step.Cursor (Stream, AdvanceResult (..), Cursory (..))
import qualified Step.Cursor as Cursor

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, uncommitted, unseen, newDoubleBuffer)
import Step.Buffer.DoubleBufferState (DoubleBufferState(..))

import Step.Buffer.HasBuffer

newtype BufferState xs x s m a =
    BufferState { runBufferState :: StateT s m a }
    deriving newtype (Functor, Applicative, Monad, MonadState s)

instance (Monad m, HasBuffer xs x s, ListLike xs x) => Cursory (BufferState xs x s m) where
    type CursoryText (BufferState xs x s m) = xs
    type CursoryChar (BufferState xs x s m) = x
    type CursoryContext (BufferState xs x s m) = (DoubleBufferState xs x m)

    cursoryRun (DoubleBufferState a) =
        use buffer
        >>= BufferState . lift . runStateT a . newDoubleBuffer
        >>= \(x, bs) -> assign buffer (view uncommitted bs) $> x

    cursoryInput =
        Cursor.stream $ DoubleBufferState $ zoom unseen $ runBufferState takeChunk

    cursoryCommit n =
        DoubleBufferState $ zoom uncommitted $ runBufferState $ dropN n

takeChunk :: HasBuffer xs x s => Monad m => BufferState xs x s m (Maybe (Nontrivial xs x))
takeChunk = use (buffer % chunks) >>= \case
    Empty -> return Nothing
    y :<| ys -> assign (buffer % chunks) ys $> Just y

dropN :: (HasBuffer xs x s, Monad m, ListLike xs x) => Positive Natural -> BufferState xs x s m AdvanceResult
dropN = fix \r n -> use (buffer % chunks) >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.dropPositive n x of
        Drop.DroppedAll -> assign (buffer % chunks) xs $> AdvanceSuccess
        Drop.DroppedPart{ Drop.remainder } -> assign (buffer % chunks) (remainder :<| xs) $> AdvanceSuccess
        Drop.Insufficient{ Drop.shortfall } -> assign (buffer % chunks) xs *> r shortfall
