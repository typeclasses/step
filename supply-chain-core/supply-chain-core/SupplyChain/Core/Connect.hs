-- | Description: functions to combine vendors and jobs

module SupplyChain.Core.Connect
  (
    {- * Vendor to job -} (>-), (>+),
    {- * Vendor to vendor -} (>->),
    {- * Referral -} joinReferral,
  )
  where

import Control.Monad ((>>=))
import Data.Functor ((<&>), fmap)

import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Referral (Referral (Referral))
import SupplyChain.Core.Vendor (Vendor (Vendor, handle))

import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.Referral as Referral
import qualified SupplyChain.Core.Vendor as Vendor

infixl 6 >-
infixl 6 >+
infixl 7 >->

{-| Modify a job with a vendor that interprets its requests -}
(>-) :: Vendor up down action -- ^ Upstream
     -> Job down action product -- ^ Downstream
     -> Job up action product
up >- down = up >+ down <&> Referral.product

{-| Connect two vendors; the first interprets requests made by the second -}
(>->) :: Vendor up middle action -- ^ Upstream
      -> Vendor middle down action -- ^ Downstream
      -> Vendor up down action
up >-> down = Vendor { handle = \request ->
    up >+ (Vendor.handle down request) <&> joinReferral }

{-| Sort of resembles what a 'Control.Monad.join' implementation for 'Referral'
    might look like, modulo a subtle difference in the types -}
joinReferral :: Referral up middle action (Referral middle down action product)
    -> Referral up down action product
joinReferral (Referral (Referral product nextDown) nextUp) =
    Referral product (nextUp >-> nextDown)

{-| Connect a vendor to a job, producing a job which returns both the product
    and a new version of the vendor.

    Use this function instead of '(>-)' if you need to attach a succession
    of jobs to one stateful vendor. -}
(>+) :: Vendor up down action -- ^ Upstream
     -> Job down action product -- ^ Downstream
     -> Job up action (Referral up down action product)
up >+ job = case job of
    Job.Pure product -> Job.Pure (Referral product up)
    Job.Perform action extract -> Job.Perform action extract <&> (`Referral` up)
    Job.Request request extract -> Vendor.handle up request <&> fmap extract
    Job.Bind a b -> up >+ a >>= \(Referral x up') -> up' >+ b x
