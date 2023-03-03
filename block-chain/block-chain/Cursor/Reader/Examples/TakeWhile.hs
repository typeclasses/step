module Cursor.Reader.Examples.TakeWhile where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type
import Miscellany

import Data.Sequence (Seq (..))
import Block (End (..), Block)
import Cursor.Interface (IsCursor)
import Control.Monad.State (State)
import SupplyChain (Job)

import qualified Data.Sequence as Seq
import qualified Block
import qualified Cursor.Interface.Orders as Cursor

takeWhile :: (item -> Bool) -> ReaderPlus up action 'Write item block (Seq block)
takeWhile f = span () (pure . f) <&> \(xs, ()) -> xs

span :: forall s up action item block. s -> (item -> State s Bool)
    -> ReaderPlus up action 'Write item block (Seq block, s)
span initial f = Reader (go (Seq.Empty, initial))
  where
    go :: Block item block => IsCursor 'Write block up =>
        (Seq block, s) -> Job up action (Seq block, s)
    go (acc, s) = Cursor.next >>= \case
        End -> pure (acc, s)
        Item x -> Block.runState s (Block.span Front (mtlToBlockState . f) x)
            & \Block.StateResult{ Block.stateResult, Block.newState } ->
                case stateResult of
                    Block.SpanNone -> pure (acc, newState)
                    Block.SpanAll -> do
                        _ <- Cursor.commitPositive (Block.length x)
                        go (acc Seq.:|> x, newState)
                    Block.SpanPart a _ -> do
                        _ <- Cursor.commitPositive (Block.length a)
                        pure (acc Seq.:|> a, newState)
