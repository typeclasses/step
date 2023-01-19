module Cursor.ResetReader.Examples
  (
    {- * Fixed length -} takePositive, takeNatural,
    {- * Amount remaining -} atEnd,
  )
  where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type
import Cursor.ResetReader.Type

import Block.Class (Take (..))
import Data.Sequence (Seq (..))
import SupplyChain (order)
import Integer (Positive, Natural)
import Cursor.Interface (next, commit)

import qualified Data.Sequence as Seq
import qualified Block.Class as Block
import qualified Integer

takePositive :: forall up action block. Positive
    -> ResetReaderPlus up action 'Write block (Advancement, Seq block)
takePositive = \n -> ResetReader (go n)
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

takeNatural :: forall up action block. Natural
    -> ResetReaderPlus up action 'Write block (Advancement, Seq block)
takeNatural n = Integer.narrow n & \case
    Just p -> takePositive p
    Nothing -> pure (AdvanceSuccess, Seq.empty)

atEnd :: ResetReaderPlus up action mode block Bool
atEnd = ResetReader $ order next <&> \case{ End -> True; Item _ -> False }
