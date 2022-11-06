module SupplyChain.Core.Vendor (Vendor (..), run, eval, alter)
    where

import Control.Monad (Monad)

import SupplyChain.Core.VendorAndSupply (Vendor (..), Supply (..))
import SupplyChain.Core.Nil (Nil)
import qualified SupplyChain.Core.Job as Job
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.VendorAndSupply as VendorAndSupply
import SupplyChain.Core.Effect (Effect)

{-| An action in which a vendor handles a single request

    The action returns a 'Supply', which contains two things:

    - The response to the request
    - A new version of the vendor
-}
run :: Monad action => Vendor Nil down action -> down product
    -> action (Supply Nil down action product)
run v r = Job.run (handle v r)

eval :: Vendor Nil down Nil -> down product -> Supply Nil down Nil product
eval v r = Job.eval (handle v r)

alter :: (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'
alter = VendorAndSupply.alterVendor
