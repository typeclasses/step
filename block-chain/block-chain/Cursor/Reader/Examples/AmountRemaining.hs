module Cursor.Reader.Examples.AmountRemaining where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type
import Cursor.Reader.Examples.Take

import Cursor.Reader.Utilities (lookAhead)
import SupplyChain (order)
import Integer (Positive, Natural)
import Cursor.Interface (next)

import qualified Integer

atEnd :: ReaderPlus up action mode item block Bool
atEnd = Reader $ order next <&> \case{ End -> True; Item _ -> False }

remainsAtLeastNatural :: Natural
    -> ReaderPlus up action mode item block Bool
remainsAtLeastNatural n = Integer.narrow n & \case
    Just p -> remainsAtLeastPositive p
    Nothing -> pure True

remainsAtLeastPositive :: Positive
    -> ReaderPlus up action mode item block Bool
remainsAtLeastPositive n =
    lookAhead (takePositive n) <&> \(a, _) -> case a of
        AdvanceSuccess -> True
        YouCanNotAdvance{} -> False
