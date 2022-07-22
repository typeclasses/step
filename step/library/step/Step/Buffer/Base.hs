{-# language FlexibleInstances, FlexibleContexts, TypeFamilies, Trustworthy #-}

module Step.Buffer.Base
  (
    Buffer, isEmpty, empty, (|>),

    -- * State operations
    takeChunk, dropN,
  )
  where

import Step.Internal.Prelude hiding (fold)
import qualified Step.Internal.Prelude as Prelude

import qualified Seq

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial.List
import qualified Step.Nontrivial.SplitAtPositive as SplitAtPositive
import Step.Nontrivial.SplitAtPositive (splitAtPositive)

import Step.Input.Cursor (Session (..))

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult)

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
    Buffer{ chunks = Seq.Empty } -> return Advance.InsufficientInput{ Advance.shortfall = n }
    Buffer{ chunks = (Seq.:<|) x xs } -> case splitAtPositive n x of
        SplitAtPositive.All -> put Buffer{ chunks = xs } $> Advance.Success
        SplitAtPositive.Split _ b -> put Buffer{ chunks = (Seq.:<|) b xs } $> Advance.Success
        SplitAtPositive.Insufficient n' -> r n'
