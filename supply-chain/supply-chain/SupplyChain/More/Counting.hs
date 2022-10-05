module SupplyChain.More.Counting where

import SupplyChain

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor (Functor, (<&>))
import Numeric.Natural (Natural)
import Prelude ((+))


data Counting i response =
    Counting_order (i response)
        -- ^ The next item, or 'Nothing' if input is exhausted
  | (response ~ Natural) => Counting_count
        -- ^ How many items have been fetched so far

type Counting :: Interface -> Interface


counting :: forall i action. Functor action =>
    Vendor i (Counting i) action

counting = go 0
  where
    go :: Natural -> Vendor i (Counting i) action
    go n = Vendor \case
        Counting_count    ->  pure $ n :-> go n
        Counting_order x  ->  order x <&> (:-> go (n + 1))
