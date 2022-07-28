{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.Buffer.BufferState (BufferOnly (..), takeChunk, dropN, bufferStateCursor) where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import Step.Cursor (Stream, AdvanceResult (..), Cursor (Cursor))
import qualified Step.Cursor as Cursor

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, uncommitted, unseen, newDoubleBuffer)

import qualified Optics

newtype BufferOnly xs x buffer m a = BufferOnly (StateT (buffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad, MonadState (buffer xs x), MonadTrans)

bufferStateCursor :: (ListLike xs x, Monad m) =>
    Cursor xs x (StateT (Buffer xs x) m) (StateT (DoubleBuffer xs x) m)
bufferStateCursor = Cursor{ Cursor.run, Cursor.input, Cursor.commit }
  where
    input = Cursor.stream $ zoom unseen takeChunk
    commit n = zoom uncommitted $ dropN n
    run a = get >>= lift . runStateT a . newDoubleBuffer >>= \(x, bs) -> put (view uncommitted bs) $> x

takeChunk :: Monad m => StateT (Buffer xs x) m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: (Monad m, ListLike xs x) => Positive Natural -> StateT (Buffer xs x) m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.dropPositive n x of
        Drop.DroppedAll -> assign chunks xs $> AdvanceSuccess
        Drop.DroppedPart{ Drop.remainder } -> assign chunks (remainder :<| xs) $> AdvanceSuccess
        Drop.Insufficient{ Drop.shortfall } -> assign chunks xs *> r shortfall
