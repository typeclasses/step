module SupplyChain.Core.VendorAndSupply (Vendor (..), Supply (..),
    alterVendor, alterSupply) where

import Data.Kind (Type)
import Data.Functor (Functor, fmap)
import Data.Function ((.))

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Kinds (Action, Interface)
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.Job as Job

-- | Makes requests, responds to requests, and performs actions

newtype Vendor (up :: Interface) (down :: Interface) (action :: Action) =
  Vendor
    { handle :: forall (product :: Type).
        down product -> Job up action (Supply up down action product) }

-- | The conclusion of a vendor's handling of a client request

data Supply (up :: Interface) (down :: Interface) (action :: Action) (product :: Type) =
  Supply
    { product :: product -- ^ The requested product
    , next :: Vendor up down action -- ^ A new vendor to handle subsequent requests
    }

deriving stock instance Functor (Supply up down action)

alterVendor :: forall up up' action action' down.
    (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'

alterVendor f Vendor{ handle } =
    Vendor{ handle = fmap (alterSupply f) . Job.alter f . handle }

alterSupply :: forall up up' action action' down product.
    (forall x. Effect up action x -> Job up' action' x)
    -> Supply up down action product -> Supply up' down action' product

alterSupply f s = s{ next = alterVendor f (next s) }
