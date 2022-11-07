module SupplyChain.Core.Connect ((>->), (>+>)) where

import Control.Monad ((>>=))
import Data.Functor ((<&>), fmap)

import SupplyChain.Core.Vendor (Vendor (Vendor, handle))
import SupplyChain.Core.Referral (Referral ((:>)))
import SupplyChain.Core.Job (Job)
import qualified SupplyChain.Core.Job as Job

infixl 7 >->
infixl 7 >+>

(>->) :: Vendor up middle action
      -> Vendor middle down action
      -> Vendor up down action
up >-> down =
  Vendor
    { handle = \request ->
        up >+> handle down request <&> \(x :> down' :> up') ->
            x :> up' >-> down'
    }

{-| Connect a vendor to a job, producing a job which returns both the product
    and a new version of the vendor. -}
(>+>) :: Vendor up down action -> Job down action product
      -> Job up action (Referral up down action product)
(>+>) up = \case
    Job.Pure product             ->  Job.Pure (product :> up)
    Job.Perform action extract   ->  Job.Perform action extract <&> (:> up)
    Job.Request request extract  ->  handle up request <&> fmap extract
    Job.Bind a b                 ->  up >+> a >>= \(x :> up') -> up' >+> b x
