{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.Buffer.BufferState (BufferState (..), takeChunk, dropN, bufferStateCursor) where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import Step.Cursor (Stream, AdvanceResult (..), Cursory (..), Cursor (Cursor))
import qualified Step.Cursor as Cursor

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, uncommitted, unseen, newDoubleBuffer)

newtype BufferState xs x buffer m a =
    BufferState { runBufferState :: StateT (buffer xs x) m a }
    deriving newtype (Functor, Applicative, Monad, MonadState (buffer xs x), MonadTrans)

instance (Monad m, ListLike xs x) => Cursory (BufferState xs x Buffer m) where
    type CursoryText (BufferState xs x Buffer m) = xs
    type CursoryChar (BufferState xs x Buffer m) = x
    type CursoryContext (BufferState xs x Buffer m) = (BufferState xs x DoubleBuffer m)
    curse = bufferStateCursor

bufferStateCursor :: (ListLike xs x, Monad m) =>
    Cursor xs x (BufferState xs x Buffer m) (BufferState xs x DoubleBuffer m)
bufferStateCursor = Cursor{ Cursor.run, Cursor.input, Cursor.commit }
  where
    run (BufferState a) =
        get
        >>= BufferState . lift . runStateT a . newDoubleBuffer
        >>= \(x, bs) -> put (view uncommitted bs) $> x

    input =
        Cursor.stream $ BufferState $ zoom unseen $ runBufferState takeChunk

    commit n =
        BufferState $ zoom uncommitted $ runBufferState $ dropN n

takeChunk :: Monad m => BufferState xs x Buffer m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: (Monad m, ListLike xs x) => Positive Natural -> BufferState xs x Buffer m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.dropPositive n x of
        Drop.DroppedAll -> assign chunks xs $> AdvanceSuccess
        Drop.DroppedPart{ Drop.remainder } -> assign chunks (remainder :<| xs) $> AdvanceSuccess
        Drop.Insufficient{ Drop.shortfall } -> assign chunks xs *> r shortfall
