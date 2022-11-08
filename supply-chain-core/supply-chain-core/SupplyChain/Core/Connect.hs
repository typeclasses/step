module SupplyChain.Core.Connect ((>->), (>-|), vendorToJob', joinReferral)
where

import Control.Monad ((>>=))
import Data.Functor ((<&>), fmap)

import SupplyChain.Core.Vendor (Vendor (Vendor, handle))
import SupplyChain.Core.Referral (Referral (Referral))
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.Vendor as Vendor
import qualified SupplyChain.Core.Referral as Referral
import qualified SupplyChain.Core.Job as Job

infixl 6 >-|
infixl 7 >->

(>-|) :: Vendor up down action -> Job down action product
    -> Job up action product
up >-| down = vendorToJob' up down <&> Referral.product

(>->) :: Vendor up middle action -> Vendor middle down action
    -> Vendor up down action
up >-> down = Vendor { handle = \request ->
    vendorToJob' up (Vendor.handle down request) <&> joinReferral }

{-| Sort of resembles what a 'Control.Monad.join' implementation for 'Referral'
    might look like, modulo a subtle difference in the types -}
joinReferral :: Referral up middle action (Referral middle down action product)
    -> Referral up down action product
joinReferral (Referral (Referral product nextDown) nextUp) =
    Referral product (nextUp >-> nextDown)

{-| Connect a vendor to a job, producing a job which returns both the product
    and a new version of the vendor.

    Use this function instead of 'vendorToJob' if you need to attach a succession
    of jobs to one stateful vendor. -}
vendorToJob' :: Vendor up down action -> Job down action product
    -> Job up action (Referral up down action product)
vendorToJob' up = \case
    Job.Pure product -> Job.Pure (Referral product up)
    Job.Perform action extract -> Job.Perform action extract <&> (`Referral` up)
    Job.Request request extract -> Vendor.handle up request <&> fmap extract
    Job.Bind a b -> vendorToJob' up a >>= \(Referral x up') -> vendorToJob' up' (b x)
