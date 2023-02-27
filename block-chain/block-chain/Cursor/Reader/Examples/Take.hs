module Cursor.Reader.Examples.Take where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type

import Data.Sequence (Seq (..))
import Integer (Positive, Natural)
import Block (Take (..), Shortfall (..), End (..), Seq1)
import SupplyChain (order)
import Cursor.Interface (next, commit)

import qualified Data.Sequence as Seq
import qualified Integer
import qualified Block

takePositive :: Positive
    -> ReaderPlus up action 'Write item block (Advancement (Seq block) (Seq1 block))
takePositive = \n -> Reader (go n)
  where
    go n = order next >>= \case
        End -> pure $ YouCanNotAdvance (Shortfall n) Seq.empty
        Item x -> case Block.take Front n x of
            TakeAll -> do
                _ <- order (commit n)
                pure $ AdvanceSuccess $ Block.singleton x
            TakePart{ taken } -> do
                _ <- order (commit (Block.length taken))
                pure $ AdvanceSuccess $ Block.singleton taken
            TakeInsufficient (Shortfall s) -> do
                _ <- order (commit (Block.length x))
                go s <&> \case
                    YouCanNotAdvance s' xs -> YouCanNotAdvance s' (x :<| xs)
                    AdvanceSuccess xs -> AdvanceSuccess (Block.push Front x xs)

takeNatural :: Natural
    -> ReaderPlus up action 'Write item block (Advancement () (), Seq block)
takeNatural n = Integer.narrow n & \case
    Nothing -> pure (AdvanceSuccess (), Seq.empty)
    Just p -> takePositive p <&> \case
        YouCanNotAdvance s xs -> (YouCanNotAdvance s (), xs)
        AdvanceSuccess xs -> (AdvanceSuccess (), Block.generalize xs)
