module Cursor.Reader.Examples.TakeNumber where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type

import Data.Sequence (Seq (..))
import Integer (Positive, Natural)
import Block (Take (..), Shortfall (..), End (..), Seq1)

import qualified Cursor.Interface.Orders as Cursor
import qualified Data.Sequence as Seq
import qualified Integer
import qualified Block

takePositive :: Positive
    -> ReaderPlus up action 'Write item block (Advancement (Seq block) (Seq1 block))
takePositive = \n -> Reader (go n Seq.Empty <&> fmap Block.assume)
  where
    go n acc = Cursor.next >>= \case
        End -> pure $ YouCanNotAdvance (Shortfall n) acc
        Item x -> case Block.take Front n x of
            TakeAll -> do
                _ <- Cursor.commitPositive n
                pure $ AdvanceSuccess (acc Seq.:|> x)
            TakePart{ taken } -> do
                _ <- Cursor.commitPositive (Block.length taken)
                pure $ AdvanceSuccess (acc Seq.:|> taken)
            TakeInsufficient (Shortfall s) -> do
                _ <- Cursor.commitPositive (Block.length x)
                go s (acc Seq.:|> x)

takeNatural :: Natural
    -> ReaderPlus up action 'Write item block (Advancement () (), Seq block)
takeNatural n = Integer.narrow n & \case
    Nothing -> pure (AdvanceSuccess (), Seq.empty)
    Just p -> takePositive p <&> \case
        YouCanNotAdvance s xs -> (YouCanNotAdvance s (), xs)
        AdvanceSuccess xs -> (AdvanceSuccess (), Block.generalize xs)
