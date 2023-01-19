module Cursor.ResetReader.Examples.AmountRemaining where

import Essentials
import Cursor.Interface.Type
import Cursor.ResetReader.Type
import Cursor.ResetReader.Examples.Take

import Cursor.ResetReader.Utilities (lookAhead)
import SupplyChain (order)
import Integer (Positive, Natural)
import Cursor.Interface (next)

import qualified Integer

atEnd :: ResetReaderPlus up action mode block Bool
atEnd = ResetReader $ order next <&> \case{ End -> True; Item _ -> False }

remainsAtLeastNatural :: Natural
    -> ResetReaderPlus up action mode block Bool
remainsAtLeastNatural n = Integer.narrow n & \case
    Just p -> remainsAtLeastPositive p
    Nothing -> pure True

remainsAtLeastPositive :: Positive
    -> ResetReaderPlus up action mode block Bool
remainsAtLeastPositive n =
    lookAhead (takePositive n) <&> \(a, _) -> case a of
        AdvanceSuccess -> True
        YouCanNotAdvance{} -> False
