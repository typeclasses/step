module Cursor.Counting.Examples
  (
    {- * Examples -} counting,
  )
  where

import Essentials
import Cursor.Interface.Type
import Cursor.Interface.Class
import Cursor.Counting.Type

import Numeric.Natural (Natural)
import SupplyChain (Vendor (Vendor), order, Referral (Referral))
import Prelude ((+), (-))

import qualified Integer.Positive as Positive

{-| Passes all 'Cursor' orders through unmodified, but stores the
    sum total of successful 'Commit' actions for retrieval via the
    'AmountCommitted' request -}
counting :: forall up block action. IsCursor 'Write block up =>
    Vendor up (Counting block) action
counting = go 0
  where
    go :: Natural -> Vendor up (Counting block) action
    go amountCommitted = Vendor \case
          AmountCommitted ->
              pure $ Referral amountCommitted $ go amountCommitted
          Order c -> case c of
              Reset -> order (liftCursor c) <&> \r -> Referral r $ go amountCommitted
              Next  -> order (liftCursor c) <&> \r -> Referral r $ go amountCommitted
              Commit n -> order (liftCursor c) <&> \r -> case r of
                  AdvanceSuccess -> Referral r $ go (amountCommitted + Positive.toNatural n)
                  YouCanNotAdvance{ shortfall } -> Referral r $ go $
                      amountCommitted + Positive.toNatural n - Positive.toNatural shortfall
