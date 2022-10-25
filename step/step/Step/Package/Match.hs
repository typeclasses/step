module Step.Package.Match (match) where

import Step.Action.Core
import Step.Error
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
import SupplyChain (Interface, Vendor (..), Job, Supply (..))
import Data.Functor
import Prelude ((+))

import qualified SupplyChain

match :: Chunk c => Any c m e a -> Any c m e (c, a)
match (Any (ResettingSequence f)) = Any $ ResettingSequence do
    _

data Counting (c :: Type) (response :: Type) =
    Order (CommittableChunkStream c response)
  | (response ~ Natural) => AmountCommitted

type Counting :: Type -> Interface

counting :: forall c action.
    Vendor (CommittableChunkStream c) (Counting c) action

counting = go 0
  where
    go :: Natural -> Vendor i (Counting c) action
    go n = Vendor \case
        AmountCommitted       ->  pure $ Supply n (go n)
        Order (I.Commit n)  ->  _
        Order I.Reset       ->  _
        Order I.NextMaybe   ->  _
