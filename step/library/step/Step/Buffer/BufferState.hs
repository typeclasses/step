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

import Step.Cursor (Stream, AdvanceResult (..))
import qualified Step.Cursor as Cursor

import Step.Buffer.BufferResult (BufferResult(..))

import Step.Buffer.Buffer (Buffer (..))

newtype BufferState xs x m a =
    BufferState { runBufferState :: StateT (Buffer xs x) m a }
    deriving newtype (Functor, Applicative, Monad)

getBuffer :: Monad m => BufferState xs x m (Buffer xs x)
getBuffer = BufferState get

setBuffer :: Monad m => Buffer xs x -> BufferState xs x m ()
setBuffer = BufferState . put

takeChunk :: Monad m => BufferState xs x m (Maybe (Nontrivial xs x))
takeChunk = BufferState $ get >>= \b -> case uncons (chunks b) of
    Nothing -> return Nothing
    Just (c, cs) -> put Buffer{ chunks = cs } $> Just c

dropN :: (Monad m, ListLike xs x) => Positive Natural -> BufferState xs x m AdvanceResult
dropN = fix \r n -> getBuffer >>= \case
    Buffer{ chunks = Seq.Empty } -> return YouCanNotAdvance{ shortfall = n }
    Buffer{ chunks = (Seq.:<|) x xs } ->
        case Nontrivial.dropPositive n x of
            Drop.DroppedAll ->
                setBuffer Buffer{ chunks = xs } $> AdvanceSuccess
            Drop.DroppedPart{ Drop.remainder } ->
                setBuffer Buffer{ chunks = (Seq.:<|) remainder xs } $> AdvanceSuccess
            Drop.Insufficient{ Drop.shortfall } ->
                setBuffer Buffer{ chunks = xs } *> r shortfall
