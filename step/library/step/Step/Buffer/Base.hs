{-# language FlexibleInstances, FlexibleContexts, Trustworthy #-}

module Step.Buffer.Base
  (
    Buffer, isEmpty, empty, (|>),

    -- * State operations
    takeChunk, dropN,
  )
  where

import Step.Internal.Prelude

import qualified Seq

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import Step.Cursor (Stream, AdvanceResult (..))
import qualified Step.Cursor as Cursor

import Step.Buffer.Result (BufferResult(..))

data Buffer xs x = Buffer { chunks :: Seq (Nontrivial xs x) }

(|>) :: Buffer xs x -> Nontrivial xs x -> Buffer xs x
Buffer xs |> x = Buffer (xs Seq.|> x)

isEmpty :: Buffer xs x -> Bool
isEmpty = Seq.null . chunks

empty :: Buffer xs x
empty = Buffer{ chunks = Seq.empty }

takeChunk :: Monad m => StateT (Buffer xs x) m (Maybe (Nontrivial xs x))
takeChunk = get >>= \b -> case uncons (chunks b) of
    Nothing -> return Nothing
    Just (c, cs) -> put Buffer{ chunks = cs } $> Just c

dropN :: (Monad m, ListLike xs x) => Positive Natural -> StateT (Buffer xs x) m AdvanceResult
dropN = fix \r n -> get >>= \case
    Buffer{ chunks = Seq.Empty } -> return YouCanNotAdvance{ shortfall = n }
    Buffer{ chunks = (Seq.:<|) x xs } ->
        case Nontrivial.dropPositive n x of
            Drop.DroppedAll ->
                put Buffer{ chunks = xs } $> AdvanceSuccess
            Drop.DroppedPart{ Drop.remainder } ->
                put Buffer{ chunks = (Seq.:<|) remainder xs } $> AdvanceSuccess
            Drop.Insufficient{ Drop.shortfall } ->
                put Buffer{ chunks = xs } *> r shortfall
