-- | Description: a /vendor/ responds to requests, makes requests, and
--                performs actions

module SupplyChain.Core.Vendor
  (
    {- * Type -} Vendor (..),
    {- * Running -} run, eval,
    {- * Alteration -} alter,
  )
  where

import Control.Monad (Monad)
import Data.Functor.Const (Const)
import Data.Void (Void)
import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.VendorAndReferral (Vendor (..), Referral (..))

import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.VendorAndReferral as VendorAndReferral

{-| An action in which a vendor handles a single request

    The action returns a 'Referral', which contains two things:

    - The response to the request
    - A new version of the vendor
-}
run :: Monad action => Vendor (Const Void) down action
    -> down product -- ^ Request
    -> action (Referral (Const Void) down action product)
run v r = Job.run (handle v r)

eval :: Vendor (Const Void) down (Const Void)
    -> down product  -- ^ Request
    -> Referral (Const Void) down (Const Void) product
eval v r = Job.eval (handle v r)

alter :: (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'
alter = VendorAndReferral.alterVendor
