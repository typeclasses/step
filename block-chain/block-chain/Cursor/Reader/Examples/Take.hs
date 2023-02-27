module Cursor.Reader.Examples.Take where

import Essentials
import Cursor.Interface.Orders
import Cursor.Reader.Type
import Cursor.Advancement

import Block (Block)
import Data.Sequence (Seq (..))
import Integer (Positive, Natural)
import Block (Take (..), Shortfall (..), End (..))
import SupplyChain (order)
import Data.Either (Either (Left, Right))
import Next (Step (..))

import qualified Data.Sequence as Seq
import qualified Integer
import qualified Block

takePositive :: Block xs xss =>
    Positive -> ReaderPlus up action item xs
    (Either (Shortfall, Maybe xss) xss)
takePositive n = next >>= \case
    End -> pure $ Left (Shortfall n, Nothing)
    Item x -> case Block.take Front n x of
        TakeAll -> pure $ Right $ Block.singleton x
        TakePart{ taken, takeRemainder } ->
            push takeRemainder $> Right (Block.singleton taken)
        TakeInsufficient (Shortfall s) -> do
            takePositive s <&> fmap (Block.push Front x)

takeNatural :: Block xs xss => Natural
    -> ReaderPlus up action item xs (Advancement, Maybe xss)
takeNatural n = Integer.narrow n & \case
    Nothing -> pure (AdvanceSuccess, Nothing)
    Just p -> takePositive p <&> \case
        Left (s, xssM) -> (YouCanNotAdvance s, xssM)
        Right xss -> (AdvanceSuccess, Just xss)
