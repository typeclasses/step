module Monitor.Recording.Vendor
  (
    {- * Vendor -} recording,
  )
  where

import Essentials

import Monitor.Interface.Type (Monitor (..))
import Monitor.Recording.Type (Recording (..))
import SupplyChain (Vendor (Vendor), Referral (Referral))

import qualified SupplyChain.Job as Job

recording :: Recording up monitor state action
    -> Vendor up (Monitor monitor up) action
recording Recording{ initial, step, extract } = go initial
  where
    go state = go'
      where
        go' = Vendor \case
          Monitor request -> do
              response <- extract request state
              pure $ Referral response $ go'
          Subject request -> do
              response <- Job.order request
              state' <- step request response state
              pure $ Referral response $ go state'
