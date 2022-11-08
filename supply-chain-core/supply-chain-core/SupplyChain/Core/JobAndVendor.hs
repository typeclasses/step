-- | Description: /job/ + /vendor/

module SupplyChain.Core.JobAndVendor
  (
    {- * Types -} Job (..), Vendor (..),
    {- * Alteration -} alterJob, alterVendor,
    {- * Conversion -} loop, once,
  )
  where

import SupplyChain.Core.Job
import SupplyChain.Core.Vendor
import SupplyChain.Core.VendorAndReferral (alterVendor)
import SupplyChain.Core.Unit
import SupplyChain.Core.Referral (Referral (..))
import SupplyChain.Core.Effect

import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.Referral as Referral

import Data.Functor

alterJob :: (forall x. Effect up action x -> Job up' action' x)
    -> Job up action product -> Job up' action' product
alterJob = Job.alter

loop :: Job up action product -> Vendor up (Unit product) action
loop j = go
  where
    go = Vendor{ handle = \Unit -> j <&> \product -> Referral product go }

once :: Vendor up (Unit product) action -> Job up action product
once v = handle v Unit <&> Referral.product
