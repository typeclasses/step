{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.Buffer.BufferState (BufferOnly (..), takeChunk, dropN, bufferStateCursor) where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import Step.Cursor (Stream, AdvanceResult (..), Cursor (Cursor))
import qualified Step.Cursor as Cursor

import Step.RST (RST (..), evalRST)

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))
import Step.Buffer.DoubleBuffer (DoubleBuffer, uncommitted, unseen, newDoubleBuffer)

import qualified Optics

newtype BufferOnly xs x buffer m a = BufferOnly (StateT (buffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad, MonadState (buffer xs x), MonadTrans)

bufferStateCursor :: forall xs x r m. (ListLike xs x, Monad m) =>
    Cursor xs x r (DoubleBuffer xs x) (Buffer xs x) m
bufferStateCursor = Cursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    init :: RST r (Buffer xs x) m (DoubleBuffer xs x)
    init = get <&> newDoubleBuffer

    input :: Stream (RST r (DoubleBuffer xs x) m) xs x
    input = Cursor.stream (zoom unseen takeChunk)

    commit :: Positive Natural -> RST r (DoubleBuffer xs x) m AdvanceResult
    commit n = zoom uncommitted (dropN n)

    extract :: RST r (DoubleBuffer xs x) m (Buffer xs x)
    extract = get <&> view uncommitted

takeChunk :: MonadState (Buffer xs x) m => m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: (MonadState (Buffer xs x) m, ListLike xs x) => Positive Natural -> m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.dropPositive n x of
        Drop.DroppedAll -> assign chunks xs $> AdvanceSuccess
        Drop.DroppedPart{ Drop.remainder } -> assign chunks (remainder :<| xs) $> AdvanceSuccess
        Drop.Insufficient{ Drop.shortfall } -> assign chunks xs *> r shortfall
