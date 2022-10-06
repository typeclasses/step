module SupplyChain.Interface.Counting
  (
    {- * Interface -} Counting (..),
    {- * Vendor -} counting,
  )
  where

import SupplyChain

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor ((<&>))
import Numeric.Natural (Natural)
import Prelude ((+))


data Counting i response =
    Order (i response)
        -- ^ The next item, or 'Nothing' if input is exhausted
  | (response ~ Natural) => Count
        -- ^ How many items have been fetched so far

type Counting :: Interface -> Interface


counting :: forall i action.
    Vendor i (Counting i) action

counting = go 0
  where
    go :: Natural -> Vendor i (Counting i) action
    go n = Vendor \case
        Count    ->  pure $ n :-> go n
        Order x  ->  order x <&> (:-> go (n + 1))
