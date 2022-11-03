module SupplyChain.Core.Vendor (Vendor (..), Supply (..), run, eval, alter)
    where

import Control.Monad (Monad)
import Data.Kind (Type)

import SupplyChain.Core.VendorAndSupply (Vendor (..), Supply (..))
import SupplyChain.Core.Kinds (Action, Interface, NoAction, NoInterface)
import qualified SupplyChain.Core.Job as Job
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.VendorAndSupply as VendorAndSupply
import SupplyChain.Core.Effect (Effect)


{-| An action in which a vendor handles a single request

    The action returns a 'Supply', which contains two things:

    - The response to the request
    - A new version of the vendor
-}

run :: forall (action :: Action) (down :: Interface) (product :: Type). Monad action =>
    Vendor NoInterface down action -> down product
    -> action (Supply NoInterface down action product)

run v r = Job.run (offer v r)


eval :: forall (down :: Interface) (product :: Type).
    Vendor NoInterface down NoAction -> down product
    -> Supply NoInterface down NoAction product

eval v r = Job.eval (offer v r)


alter :: forall up up' action action' down.
    (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'

alter = VendorAndSupply.alterVendor