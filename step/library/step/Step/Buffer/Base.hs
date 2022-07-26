{-# language FlexibleInstances, FlexibleContexts, Trustworthy #-}

module Step.Buffer.Base
  (
    Buffer, isEmpty, empty, (|>),

    -- * State operations
    takeChunk, dropN, drink,
  )
  where

import Step.Internal.Prelude

import qualified Seq

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult)

import Step.Input.Stream (Stream)
import qualified Step.Input.Stream as Stream

import Step.Buffer.Result (BufferResult(..))

data Buffer text char = Buffer { chunks :: Seq (Nontrivial text char) }

(|>) :: Buffer text char -> Nontrivial text char -> Buffer text char
Buffer xs |> x = Buffer (xs Seq.|> x)

isEmpty :: Buffer text char -> Bool
isEmpty = Seq.null . chunks

empty :: Buffer text char
empty = Buffer{ chunks = Seq.empty }

takeChunk :: Monad m => StateT (Buffer text char) m (Maybe (Nontrivial text char))
takeChunk = get >>= \b -> case uncons (chunks b) of
    Nothing -> return Nothing
    Just (c, cs) -> put Buffer{ chunks = cs } $> Just c

dropN :: (Monad m, ListLike text char) => Positive Natural -> StateT (Buffer text char) m AdvanceResult
dropN = fix \r n -> get >>= \case
    Buffer{ chunks = Seq.Empty } ->
        return Advance.InsufficientInput{ Advance.shortfall = n }
    Buffer{ chunks = (Seq.:<|) x xs } ->
        case Nontrivial.dropPositive n x of
            Drop.DroppedAll ->
                put Buffer{ chunks = xs } $> Advance.Success
            Drop.DroppedPart{ Drop.remainder } ->
                put Buffer{ chunks = (Seq.:<|) remainder xs } $> Advance.Success
            Drop.Insufficient{ Drop.shortfall } ->
                put Buffer{ chunks = xs } *> r shortfall

drink :: Monad m => Stream m (Nontrivial text char) -> StateT (Buffer text char) m BufferResult
drink xs = lift (Stream.next xs) >>= \case
    Nothing -> return NothingToBuffer
    Just x -> modify' (|> x) $> BufferedMore
