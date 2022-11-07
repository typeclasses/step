module SupplyChain.Core.Vendor (Vendor (..), run, eval, alter)
    where

import Control.Monad (Monad)
import Data.Functor.Const (Const)
import Data.Void (Void)

import SupplyChain.Core.VendorAndSupply (Vendor (..), Supply (..))
import qualified SupplyChain.Core.Job as Job
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.VendorAndSupply as VendorAndSupply
import SupplyChain.Core.Effect (Effect)

{-| An action in which a vendor handles a single request

    The action returns a 'Supply', which contains two things:

    - The response to the request
    - A new version of the vendor
-}
run :: Monad action => Vendor (Const Void) down action -> down product
    -> action (Supply (Const Void) down action product)
run v r = Job.run (handle v r)

eval :: Vendor (Const Void) down (Const Void) -> down product
    -> Supply (Const Void) down (Const Void) product
eval v r = Job.eval (handle v r)

alter :: (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'
alter = VendorAndSupply.alterVendor
