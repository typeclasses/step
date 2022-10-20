{- |

Description: The 'Counting' interface, for a vendor that keeps track of how many requests it has served

-}

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
    Vendor i (Counting i) action Natural

counting = go 0
  where
    go :: Natural -> Vendor i (Counting i) action Natural
    go n = Vendor \case
        Count    ->  pure $ Supply n n (go n)
        Order x  ->  order x <&> \product -> Supply product (n + 1) (go (n + 1))
