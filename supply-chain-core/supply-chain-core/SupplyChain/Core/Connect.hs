module SupplyChain.Core.Connect (vendorToJob, vendorToVendor, vendorToJob')
    where

import Control.Monad ((>>=))
import Data.Functor ((<&>), fmap)
import Data.Kind (Type)

import SupplyChain.Core.Vendor (Vendor (Vendor))
import SupplyChain.Core.Supply (Supply (Supply))
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.Vendor as Vendor
import qualified SupplyChain.Core.Supply as Supply
import qualified SupplyChain.Core.Job as Job
import SupplyChain.Core.Kinds (Interface, Action)


vendorToJob :: forall (up :: Interface) (down :: Interface) (action :: Action)
    (product :: Type). Vendor up down action -> Job down action product
    -> Job up action product

vendorToJob up down = vendorToJob' up down <&> Supply.product


vendorToVendor :: forall (up :: Interface) (middle :: Interface)
    (down :: Interface) (action :: Action). Vendor up middle action
    -> Vendor middle down action -> Vendor up down action

vendorToVendor up down = Vendor \request ->
    vendorToJob' up (Vendor.handle down request) <&> joinSupply


{-| Sort of resembles what a 'Control.Monad.join' implementation for 'Supply'
    might look like, modulo a subtle difference in the types -}

joinSupply :: forall (up :: Interface) (middle :: Interface) (down :: Interface)
    (action :: Action) (product :: Type). Supply up middle action
    (Supply middle down action product) -> Supply up down action product

joinSupply (Supply (Supply product nextDown) nextUp) =
    Supply product (vendorToVendor nextUp nextDown)


{-| Connect a vendor to a job, producing a job which returns both the product
    and a new version of the vendor.

    Use this function instead of 'vendorToJob' if you need to attach a succession
    of jobs to one stateful vendor. -}

vendorToJob' :: forall (up :: Interface) (down :: Interface) (action :: Action)
    (product :: Type). Vendor up down action -> Job down action product
    -> Job up action (Supply up down action product)

vendorToJob' up = \case
    Job.Pure product -> Job.Pure (Supply product up)
    Job.Perform action extract -> Job.Perform action extract <&> (`Supply` up)
    Job.Request request extract -> Vendor.handle up request <&> fmap extract
    Job.Bind a b -> vendorToJob' up a >>= \(Supply x up') -> vendorToJob' up' (b x)
