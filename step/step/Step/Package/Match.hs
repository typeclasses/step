module Step.Package.Match (match) where

import Step.Action.Core
import Step.Error
import Step.Walk (Walk (..))
import Step.Package.Failure
import Step.Chunk
import Step.Interface

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Data.Kind (Type)
import Control.Applicative (pure)
import Data.Bool (Bool (..))
import Data.Function
import Numeric.Natural
import SupplyChain (Interface, Vendor (..), Factory, Supply (..))
import Data.Functor
import Prelude ((+))

import qualified SupplyChain

match :: Chunk c => Any c m e a -> Any c m e (c, a)
match (Any (Walk f)) = Any $ Walk do
    _

data Counting (c :: Type) (response :: Type) =
    Order (Step 'RW c response)
  | (response ~ Natural) => AmountCommitted

type Counting :: Type -> Interface

counting :: forall c action.
    Vendor (Step 'RW c) (Counting c) action

counting = go 0
  where
    go :: Natural -> Vendor i (Counting c) action
    go n = Vendor \case
        AmountCommitted       ->  pure $ Supply n (go n)
        Order (StepCommit n)  ->  _
        Order StepReset       ->  _
        Order StepNext        ->  _
