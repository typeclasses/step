{-# language FlexibleInstances, FlexibleContexts, NamedFieldPuns, TypeFamilies, Trustworthy #-}

module Step.Buffer.Base
  (
    Buffer, singleton, isEmpty, empty, fold,

    -- * State operations
    takeChunk, TakeStringResult (..), dropN,
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

instance Semigroup (Buffer text char) where
    a <> b = Buffer{ chunks = chunks a <> chunks b }

singleton :: Nontrivial text char -> Buffer text char
singleton x = Buffer{ chunks = Seq.singleton x }

isEmpty :: Buffer text char -> Bool
isEmpty = Seq.null . chunks

empty :: Buffer text char
empty = Buffer{ chunks = Seq.empty }

fold :: Monoid text => Buffer text char -> text
fold = Prelude.fold . fmap Nontrivial.generalize . chunks

unconsChunk :: Buffer text char -> Maybe (Nontrivial text char, Buffer text char)
unconsChunk b = case uncons (chunks b) of
    Nothing -> Nothing
    Just (c, cs) -> Just (c, Buffer{ chunks = cs } )

data TakeStringResult text char =
    TakeStringFail
  | TakeStringPartial (Nontrivial text char) -- ^ What further needed text remains
  | TakeStringSuccess

takeChunk :: Monad m => StateT (Buffer text char) m (Maybe (Nontrivial text char))
takeChunk = do
    b <- get
    case unconsChunk b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

dropN :: (Monad m, ListLike text char) => Positive Natural -> StateT (Buffer text char) m AdvanceResult
dropN = fix \r n -> get >>= \case
    Buffer{ chunks = Seq.Empty } -> return Advance.InsufficientInput{ Advance.shortfall = n }
    Buffer{ chunks = (Seq.:<|) x xs } -> case splitAtPositive n x of
        SplitAtPositive.All -> put Buffer{ chunks = xs } $> Advance.Success
        SplitAtPositive.Split _ b -> put Buffer{ chunks = (Seq.:<|) b xs } $> Advance.Success
        SplitAtPositive.Insufficient n' -> r n'
