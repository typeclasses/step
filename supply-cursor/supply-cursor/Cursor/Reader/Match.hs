module Cursor.Reader.Match
  (
    match,
  )
  where

import Essentials
import Cursor.Interface.Type
import Cursor.Reader.Type

import Block.Class (Block)
import Data.Sequence (Seq)
import Numeric.Natural (Natural)
import SupplyChain (Vendor (Vendor), order, Referral (Referral))
import Prelude ((+))

import qualified Cursor.Feed.Examples as Feed
import qualified Integer.Positive as Positive

match :: ReaderPlus up action mode block product
    -> ReaderPlus up action mode block (Seq block, product)
match = do
    (n, a) <- act @Any \r -> privateDoubleBuffer >-> counting >- do
        ea <- Vendor.map Order >- resettingSequenceJob (x r)
        n <- SupplyChain.order AmountCommitted
        pure $ ea <&> \a -> (n, a)
    c <- tryTakeNatural n
    P.pure (c, a)

data Counting mode block product =
    Order (Cursor mode block product)
  | (product ~ Natural) => AmountCommitted

counting :: forall mode block action. Block block =>
    Vendor (Cursor mode block) (Counting mode block) action
counting = go 0
  where
    go :: Natural -> Vendor (Cursor mode block) (Counting mode block) action
    go amountCommitted = Vendor \case
          AmountCommitted -> pure $ amountCommitted `Referral` go amountCommitted
          Order Reset -> order Reset <&> (`Referral` go amountCommitted)
          Order Next -> order Next <&> (`Referral` go amountCommitted)
          Order c@(Commit n) -> order c <&> (`Referral` go (amountCommitted + Positive.toNatural n))
