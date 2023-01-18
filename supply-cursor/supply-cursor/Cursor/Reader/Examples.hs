module Cursor.Reader.Examples
  (
    takePositive,
    takeNatural,
  )
  where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type

import Block.Class (Block, Take (..))
import Data.Sequence (Seq (..))
import SupplyChain (order)
import Integer (Positive, Natural)
import Cursor.Interface (next, commit)

import qualified Data.Sequence as Seq
import qualified Block.Class as Block
import qualified Integer

takePositive :: forall up action block. Block block =>
    Positive -> ReaderPlus up action 'Write block (Advancement, Seq block)
takePositive = go
  where
    go :: Positive -> ReaderPlus up action 'Write block (Advancement, Seq block)
    go n = order next >>= \case
        End -> pure (YouCanNotAdvance{ shortfall = n }, Seq.empty)
        Item x -> case Block.take n x of
            TakeAll -> do
                _ <- order (commit n)
                pure (AdvanceSuccess, Seq.singleton x)
            TakePart{ takePart } -> do
                _ <- order (commit (Block.length takePart))
                pure (AdvanceSuccess, Seq.singleton takePart)
            TakeInsufficient{ takeShortfall } -> do
                _ <- order (commit (Block.length x))
                go takeShortfall <&> \(a, xs) -> (a, x :<| xs)

takeNatural:: forall up action block. Block block =>
    Natural -> ReaderPlus up action 'Write block (Advancement, Seq block)
takeNatural n = Integer.narrow n & \case
    Just p -> takePositive p
    Nothing -> pure (AdvanceSuccess, Seq.empty)
