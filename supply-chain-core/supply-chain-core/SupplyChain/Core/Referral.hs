-- | Description: a /referral/ consists of a product and a new vendor

module SupplyChain.Core.Referral
  (
    {- * Type -} Referral (..),
    {- * Alteration -} alter,
  )
  where

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.VendorAndReferral (Referral)

import qualified SupplyChain.Core.VendorAndReferral as VendorAndReferral

alter :: (forall x. Effect up action x -> Job up' action' x)
    -> Referral up down action product -> Referral up' down action' product
alter = VendorAndReferral.alterReferral
