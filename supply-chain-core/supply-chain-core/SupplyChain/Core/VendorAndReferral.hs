{-

Vender and Referral are defined in one module together because their definitions
are mutually recursive. This module is not exported by the package; instead, the
definitions from this module are re-exported by "SupplyChain.Core.Vendor" and
"SupplyChain.Core.Referral".

-}

module SupplyChain.Core.VendorAndReferral (Vendor (..), Referral (..),
alterVendor, alterReferral) where

import Data.Functor (Functor, fmap)
import Data.Function ((.))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.Job as Job

-- | Makes requests, responds to requests, and performs actions
newtype Vendor up down action =
  Vendor
    { handle :: forall product.
        down product -> Job up action (Referral up down action product) }

-- | The conclusion of a vendor's handling of a client request
data Referral up down action product =
  Referral
    { product :: product -- ^ The requested product
    , next :: Vendor up down action -- ^ A new vendor to handle subsequent requests
    }

deriving instance Functor (Referral up down action)
deriving instance Foldable (Referral up down action)
deriving instance Traversable (Referral up down action)

alterVendor :: (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'
alterVendor f v =
    Vendor{ handle = fmap (alterReferral f) . Job.alter f . handle v }

alterReferral :: (forall x. Effect up action x -> Job up' action' x)
    -> Referral up down action product -> Referral up' down action' product
alterReferral f s = s{ next = alterVendor f (next s) }
