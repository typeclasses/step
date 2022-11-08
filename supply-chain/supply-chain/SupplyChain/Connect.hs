module SupplyChain.Connect (Connect (..),
    vendorToJob, vendorToVendor, vendorToJob', jobOfVendor) where

import SupplyChain.Core.Connect (vendorToJob, vendorToJob', vendorToVendor)
import SupplyChain.Core.Types (Vendor, Job)
import SupplyChain.Core.Vendor (Vendor (..))

import Control.Monad ((>>=))

class Connect up down action client result | up client -> result,
    client -> down action, result -> up action
  where
    {-| Generalizes 'vendorToJob' and 'vendorToVendor'

        This operation is associative; if @a@ and @b@ are vendors
        and @c@ is a job, then @(a >-> b) >-> c@ is the same supply
        chain as @a >-> (b >-> c)@.
    -}
    (>->) :: Vendor up down action -> client -> result

instance Connect up down action
    (Job down action product) (Job up action product)
  where
    (>->) = vendorToJob

instance Connect up middle action
    (Vendor middle down action) (Vendor up down action)
  where
    (>->) = vendorToVendor

jobOfVendor :: Job up action (Vendor up down action) -> Vendor up down action
jobOfVendor j = Vendor{ handle = \request -> j >>= \v -> handle v request }
