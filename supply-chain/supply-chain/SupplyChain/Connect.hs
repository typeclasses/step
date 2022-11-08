module SupplyChain.Connect ((>->),
    vendorToJob, vendorToJob', loop, once, Unit (..)) where

import SupplyChain.Core.Connect (vendorToJob, vendorToJob', vendorToVendor)
import SupplyChain.Core.Types (Vendor, Job)
import SupplyChain.Vendor (Vendor (..))
import SupplyChain.Referral (Referral (..))
import qualified SupplyChain.Referral as Referral

import Data.Functor

(>->) :: Vendor up middle action -> Vendor middle down action -> Vendor up down action
(>->) = vendorToVendor

data Unit a b = a ~ b => Unit

loop :: Job up action product -> Vendor up (Unit product) action
loop j = go where go = Vendor{ handle = \Unit -> j <&> \product -> Referral product go }

once :: Vendor up (Unit product) action -> Job up action product
once v = handle v Unit <&> Referral.product
