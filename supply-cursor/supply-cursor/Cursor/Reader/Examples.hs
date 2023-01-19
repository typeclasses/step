module Cursor.Reader.Examples where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type

import Block.Class (Take (..))
import Data.Sequence (Seq (..))
import SupplyChain (order)
import Integer (Positive)
import Cursor.Interface (next, commit)

import qualified Data.Sequence as Seq
import qualified Block.Class as Block

takePositive :: Positive
    -> ReaderPlus up action 'Write block (Advancement, Seq block)
takePositive n = order next >>= \case
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
            takePositive takeShortfall <&> \(a, xs) -> (a, x :<| xs)
