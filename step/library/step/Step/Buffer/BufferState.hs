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

import qualified Optics

newtype BufferOnly xs x buffer m a = BufferOnly (StateT (buffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad, MonadState (buffer xs x), MonadTrans)

bufferStateCursor :: forall xs x r s m. (ListLike xs x, Monad m) =>
    Lens' s (Buffer xs x) -> Cursor xs x r s m
bufferStateCursor bufferLens = Cursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    unseenLens :: Lens' (Buffer xs x, s) (Buffer xs x)
    unseenLens = Optics._1

    uncommittedLens :: Lens' (Buffer xs x, s) (Buffer xs x)
    uncommittedLens = Optics._2 % bufferLens

    init :: s -> Buffer xs x
    init = view bufferLens

    extract :: Buffer xs x -> s -> s
    extract _ = id

    input :: Stream r (Buffer xs x, s) m xs x
    input = Cursor.Stream (zoom unseenLens takeChunk)

    commit :: Positive Natural -> RST r (Buffer xs x, s) m AdvanceResult
    commit n = zoom uncommittedLens (dropN n)

takeChunk :: Monad m => RST r (Buffer xs x) m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: Monad m => ListLike xs x => Positive Natural -> RST r (Buffer xs x) m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.dropPositive n x of
        Drop.DroppedAll -> assign chunks xs $> AdvanceSuccess
        Drop.DroppedPart{ Drop.remainder } -> assign chunks (remainder :<| xs) $> AdvanceSuccess
        Drop.Insufficient{ Drop.shortfall } -> assign chunks xs *> r shortfall
