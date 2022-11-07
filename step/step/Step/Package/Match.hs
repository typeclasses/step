module Step.Package.Match (match) where

import Step.Action.Core
import Step.Package.Failure
import Step.Chunk
import Step.Interface
import qualified Step.Interface.Core as I

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Data.Kind (Type)
import Control.Applicative (pure)
import Data.Bool (Bool (..))
import Data.Function
import Numeric.Natural
import SupplyChain (Vendor (..), Job, Referral (..))
import Data.Functor
import Prelude ((+))

import qualified SupplyChain

match :: Chunk c => Any c m r e a -> Any c m r e (c, a)
match (Any x) = Any \r -> ResettingSequenceJob do
    _

data Counting (c :: Type) (response :: Type) =
    Order (CommittableChunkStream c response)
  | (response ~ Natural) => AmountCommitted

counting :: forall c action.
    Vendor (CommittableChunkStream c) (Counting c) action

counting = go 0
  where
    go :: Natural -> Vendor i (Counting c) action
    go n = Vendor \case
        AmountCommitted       ->  pure $ Referral n (go n)
        Order (I.Commit n)  ->  _
        Order I.Reset       ->  _
        Order I.NextMaybe   ->  _
