module Step.Package.Match (match) where

import Step.Action.Core
import Step.Chunk
import Step.Interface
import qualified Step.Interface.Core as I
import Step.Package.FixedLength (tryTakeNatural)
import Step.Buffer.Private (privateDoubleBuffer)

import qualified Step.Do as P

import Data.Kind (Type)
import Control.Applicative (pure)
import Data.Function
import Data.Maybe (Maybe (..))
import Numeric.Natural
import SupplyChain (Vendor (..), Referral (..), (>->), order, (>-))
import Data.Functor
import Prelude ((+))

import qualified Optics
import qualified NatOptics.Positive as Positive

import qualified SupplyChain.Vendor as Vendor

match :: Chunk c => Any c m r a -> Any c m r (Maybe c, a)
match (Any x) =
  P.do
    (n, a) <- act @Any \r -> privateDoubleBuffer >-> counting >- do
        ea <- Vendor.map Order >- resettingSequenceJob (x r)
        n <- SupplyChain.order AmountCommitted
        pure $ ea <&> \a -> (n, a)
    c <- tryTakeNatural n
    P.pure (c, a)

data Counting (c :: Type) (response :: Type) =
    Order (CommittableChunkStream c response)
  | (response ~ Natural) => AmountCommitted

counting :: forall c action. Chunk c =>
    Vendor (CommittableChunkStream c) (Counting c) action
counting = go 0
  where
    go :: Natural -> Vendor (CommittableChunkStream c) (Counting c) action
    go amountCommitted = Vendor
      { handle = \case
          AmountCommitted -> pure $ amountCommitted `Referral` go amountCommitted
          Order I.Reset -> order I.Reset <&> (`Referral` go amountCommitted)
          Order I.NextMaybe -> order I.NextMaybe <&> (`Referral` go amountCommitted)
          Order c@(I.Commit n) -> order c <&> (`Referral` go (amountCommitted + Optics.review Positive.refine n))
      }
