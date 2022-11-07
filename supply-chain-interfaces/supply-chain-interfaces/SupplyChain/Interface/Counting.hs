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
  | (response ~ Natural) => Count
        -- ^ How many items have been fetched so far

counting :: forall i action. Vendor i (Counting i) action
counting = go 0
  where
    go :: Natural -> Vendor i (Counting i) action
    go n = Vendor \case
        Count    ->  pure $ Referral n (go n)
        Order x  ->  order x <&> (`Referral` go (n + 1))
