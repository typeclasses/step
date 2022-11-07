module SupplyChain.Connect ((>->), (>+>)) where

import SupplyChain.Core.Connect (vendorToJob, vendorToJob', (>->))
import SupplyChain.Core.Supply (Supply)
import SupplyChain.Core.Types (Vendor, Job)

import Data.Functor.Const (Const)
