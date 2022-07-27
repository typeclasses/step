{-# language FlexibleContexts, DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.BufferState
  (
    BufferState (..),
    takeChunk,
    dropN,
  )
  where

import Step.Internal.Prelude

import qualified Seq

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import Step.Cursor (Stream, AdvanceResult (..), Cursory (..))
import qualified Step.Cursor as Cursor

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferResult (BufferResult(..))
import Step.Buffer.DoubleBuffer (DoubleBuffer (..), uncommittedLens, unseenLens)
import Step.Buffer.DoubleBufferState (DoubleBufferState(..))

newtype BufferState xs x m a =
    BufferState { runBufferState :: StateT (Buffer xs x) m a }
    deriving newtype (Functor, Applicative, Monad, MonadState (Buffer xs x))

instance (Monad m, ListLike xs x) => Cursory (BufferState xs x m) where
    type CursoryText (BufferState xs x m) = xs
    type CursoryChar (BufferState xs x m) = x
    type CursoryContext (BufferState xs x m) = (DoubleBufferState xs x m)

    cursoryRun (DoubleBufferState a) = do
        b <- BufferState get
        (x, bs) <- BufferState $ lift (runStateT a (DoubleBuffer b b))
        put (view uncommittedLens bs)
        return x

    cursoryInput = Cursor.stream $ DoubleBufferState $ zoom unseenLens (runBufferState takeChunk)

    cursoryCommit n = DoubleBufferState $ zoom uncommittedLens (runBufferState (dropN n))

takeChunk :: Monad m => BufferState xs x m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> do
        assign chunks ys
        return (Just y)

dropN :: (Monad m, ListLike xs x) => Positive Natural -> BufferState xs x m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.dropPositive n x of
        Drop.DroppedAll -> do
            assign chunks xs
            return AdvanceSuccess
        Drop.DroppedPart{ Drop.remainder } -> do
            assign chunks (remainder :<| xs)
            return AdvanceSuccess
        Drop.Insufficient{ Drop.shortfall } -> do
            assign chunks xs
            r shortfall
