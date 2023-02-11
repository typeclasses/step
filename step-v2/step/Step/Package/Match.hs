module Step.Package.Match (match) where

import Essentials
import Step.Action.Core
import Block.Class.Class
import Step.Interface
import Step.LeftRight

import Step.Package.FixedLength (tryTakeNatural)
import Step.Buffer.Private (privateDoubleBuffer)

import qualified Step.Interface.Core as I
import qualified Step.Do as P

import Numeric.Natural
import SupplyChain (Vendor (..), Referral (..), (>->), order, (>-))
import Prelude ((+))

import qualified Integer.Positive as Positive

import qualified SupplyChain.Vendor as Vendor

class Match p where
    match :: Block c => p c m r a -> p c m r (Maybe c, a)

instance Match Any where
    match (Any x) = P.do
        (n, a) <- act @Any \r -> privateDoubleBuffer >-> counting >- do
            ea <- Vendor.map Order >- resettingSequenceJob (x r)
            n <- SupplyChain.order AmountCommitted
            pure $ ea <&> \a -> (n, a)
        c <- tryTakeNatural n
        P.pure (c, a)

instance Match Sure where
    match (Sure x) = P.do
        (n, a) <- act @Sure \r -> privateDoubleBuffer >-> counting >- do
            a <- Vendor.map Order >- resettingSequenceJob (x r)
            n <- SupplyChain.order AmountCommitted
            pure (right (n, a))
        c <- tryTakeNatural n
        P.pure (c, a)

instance Match Atom where
    match (Atom x) = Atom (fmap match x)

data Counting (c :: Type) (response :: Type) =
    Order (CommittableChunkStream c response)
  | (response ~ Natural) => AmountCommitted

counting :: forall c action. Block c =>
    Vendor (CommittableChunkStream c) (Counting c) action
counting = go 0
  where
    go :: Natural -> Vendor (CommittableChunkStream c) (Counting c) action
    go amountCommitted = Vendor
      { handle = \case
          AmountCommitted -> pure $ amountCommitted `Referral` go amountCommitted
          Order I.Reset -> order I.Reset <&> (`Referral` go amountCommitted)
          Order I.NextMaybe -> order I.NextMaybe <&> (`Referral` go amountCommitted)
          Order c@(I.Commit n) -> order c <&> (`Referral` go (amountCommitted + Positive.toNatural n))
      }
