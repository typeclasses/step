module Cursor.Reader.Examples.Take where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type

import Data.Sequence (Seq (..))
import Integer (Positive, Natural)
import Block.Class.Class (Take (..))
import SupplyChain (order)
import Cursor.Interface (next, commit)

import qualified Data.Sequence as Seq
import qualified Integer
import qualified Block.Class as Block

takePositive :: Positive
    -> ReaderPlus up action 'Write block (Advancement, Seq block)
takePositive = \n -> Reader (go n)
  where
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

takeNatural :: Natural
    -> ReaderPlus up action 'Write block (Advancement, Seq block)
takeNatural n = Integer.narrow n & \case
    Just p -> takePositive p
    Nothing -> pure (AdvanceSuccess, Seq.empty)
