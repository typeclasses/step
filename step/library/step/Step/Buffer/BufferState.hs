{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.Buffer.BufferState (BufferOnly (..), takeChunk, dropN, bufferStateCursor) where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial

import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor))
import qualified Step.Cursor as Cursor

import Step.RST (RST (..), evalRST)

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult (..))

import qualified Optics

newtype BufferOnly xs x buffer m a = BufferOnly (StateT (buffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad, MonadState (buffer xs x), MonadTrans)

bufferStateCursor :: forall xs x r s m. (Monad m) =>
    Lens' s (Buffer xs x) -> ReadWriteCursor xs x r s m
bufferStateCursor bufferLens = ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init = use bufferLens
    input = Cursor.Stream (zoom Cursor.ephemeralStateLens takeChunk)
    commit n = zoom (Cursor.committedStateLens % bufferLens) (dropN n)

takeChunk :: Monad m => RST r (Buffer xs x) m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: Monad m => Positive Natural -> RST r (Buffer xs x) m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.drop x n of
        Nontrivial.DroppedAll -> assign chunks xs $> AdvanceSuccess
        Nontrivial.DroppedPart{ Nontrivial.dropRemainder } -> assign chunks (dropRemainder :<| xs) $> AdvanceSuccess
        Nontrivial.InsufficientToDrop{ Nontrivial.dropShortfall } -> assign chunks xs *> r dropShortfall
