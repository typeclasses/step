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

newtype BufferState xs x m a =
    BufferState { runBufferState :: StateT (Buffer xs x) m a }
    deriving newtype (Functor, Applicative, Monad, MonadState (Buffer xs x))

instance (Monad m, ListLike xs x) => Cursory (BufferState xs x m) where
    type CursoryText (BufferState xs x m) = xs
    type CursoryChar (BufferState xs x m) = x
    type CursoryContext (BufferState xs x m) = (DoubleBufferState xs x m)

    cursoryRun (DoubleBufferState a) =
        get
        >>= BufferState . lift . runStateT a . newDoubleBuffer
        >>= \(x, bs) -> put (view uncommitted bs) $> x

    cursoryInput =
        Cursor.stream $ DoubleBufferState $ zoom unseen $ runBufferState takeChunk

    cursoryCommit n =
        DoubleBufferState $ zoom uncommitted $ runBufferState $ dropN n

takeChunk :: Monad m => BufferState xs x m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: (Monad m, ListLike xs x) => Positive Natural -> BufferState xs x m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.dropPositive n x of
        Drop.DroppedAll -> assign chunks xs $> AdvanceSuccess
        Drop.DroppedPart{ Drop.remainder } -> assign chunks (remainder :<| xs) $> AdvanceSuccess
        Drop.Insufficient{ Drop.shortfall } -> assign chunks xs *> r shortfall
