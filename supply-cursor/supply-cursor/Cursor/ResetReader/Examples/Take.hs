module Cursor.ResetReader.Examples.Take where

import Essentials
import Cursor.Interface.Type
import Cursor.ResetReader.Type

import Data.Sequence (Seq (..))
import Integer (Positive, Natural)

import qualified Data.Sequence as Seq
import qualified Integer
import qualified Cursor.Reader.Examples as Reader

takePositive :: Positive
    -> ResetReaderPlus up action 'Write block (Advancement, Seq block)
takePositive n = ResetReader (Reader.takePositive n)

takeNatural :: Natural
    -> ResetReaderPlus up action 'Write block (Advancement, Seq block)
takeNatural n = Integer.narrow n & \case
    Just p -> takePositive p
    Nothing -> pure (AdvanceSuccess, Seq.empty)
