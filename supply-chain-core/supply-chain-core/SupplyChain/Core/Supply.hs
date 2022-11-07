module SupplyChain.Core.Supply (Supply (..), alter) where

import SupplyChain.Core.VendorAndSupply (Supply (..))
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.VendorAndSupply as VendorAndSupply
import SupplyChain.Core.Effect (Effect)

alter :: (forall x. Effect up action x -> Job up' action' x)
    -> Supply up down action product -> Supply up' down action' product
alter = VendorAndSupply.alterSupply
