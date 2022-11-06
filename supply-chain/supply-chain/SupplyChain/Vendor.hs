module SupplyChain.Vendor
  (
    Vendor (Vendor, handle), run, eval, alter,
    {- ** Some simple vendors -} function, action, absurd, map,
  )
  where

import SupplyChain.Core.Nil (Nil)
import SupplyChain.Core.Supply (Supply (..))
import SupplyChain.Core.Vendor (Vendor (..), run, eval, alter)

import qualified SupplyChain.Core.Job as Job

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor ((<&>))

-- | A simple stateless vendor that responds to each request by applying a pure function
function :: (forall response. down response -> response) -> Vendor up down action
function f = go where go = Vendor \x -> pure $ Supply (f x) go

-- | A simple stateless vendor that responds to each request by applying an effectful function
action :: (forall response. down response -> action response) -> Vendor up down action
action f = go where go = Vendor \x -> Job.perform (f x) <&> (`Supply` go)

absurd :: Vendor up Nil action
absurd = Vendor \case{}

map :: (forall x. down x -> up x) -> Vendor up down action
map f = go where go = Vendor \x -> Job.order (f x) <&> (`Supply` go)
