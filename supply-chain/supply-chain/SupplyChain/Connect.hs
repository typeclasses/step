module SupplyChain.Connect ((>->), (>+>), (:+) (..)) where

import SupplyChain.Core.Connect (vendorToJob, vendorToJob', vendorToVendor)
import SupplyChain.Core.Supply (Supply)
import SupplyChain.Core.Types (Vendor, Job)

import Data.Functor.Const (Const)

(>->) :: Vendor up middle action -> Vendor middle down action -> Vendor up down action
(>->) = vendorToVendor

(>+>) :: Vendor up middle action -> Vendor middle down action
    -> Vendor up (down :+ Vendor up down action) action
(>+>) = _

data (:+) i extra product where (:+) :: product ~ (p, extra) => i p -> (:+) i extra product
