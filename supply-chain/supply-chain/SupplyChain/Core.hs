{- |

Description: The innermost module of the supply-chain library

This module's chief virtue is minimalism. It aims to implement
only the most significant contributions of the library. If you
are new to supply-chain, start with "SupplyChain" instead,
which is better documented and somewhat more abstract.

-}
module SupplyChain.Core where

import Control.Applicative (Applicative (pure, (<*>), (*>)))
import Control.Arrow ((>>>))
import Control.Monad (Monad ((>>=)), (>=>))
import Data.Function (($), (.), const)
import Data.Functor (Functor (fmap), (<&>))
import Data.Kind (Type)
import Data.Functor.Const (Const (..))
import Data.Maybe (Maybe (..))
import Data.Void (absurd, Void)

import qualified Control.Monad as Monad

import qualified SupplyChain.Core.FreeMonad as M



{-
-- | Run a job in its 'Action' context

runJob :: forall (action :: Action) (product :: Type). Monad action =>
    Job NoInterface action product -> action product

runJob = go
  where
    go :: forall a. Job NoInterface action a -> a -> action b
    go = \case
      -- If the root of the tree is left-associated, perform a rotation before proceeding.
      JobCompose (JobCompose step1 step2) step3 -> go (JobCompose step1 (JobCompose step2 step3))

      JobCompose step1 step2 -> go step1 >=> go step2
      JobPure x ->
      Atom (Pure f) -> pure . f
      Atom (Effect f g) -> case f x of
          Perform p -> p <&> g
          Request (Const y) -> absurd y



-- | Run a job that performs no actions

evalJob :: forall (param :: Type) (product :: Type).
    Job NoInterface NoAction param product -> param -> product

evalJob =  go
  where
    go :: forall a b. Job NoInterface NoAction a b -> a -> b
    go = \case
      Compose (Compose step1 step2) step3 -> go (Compose step1 (Compose step2 step3))

      Compose step1 step2 -> go step1 >=> go step2
      Atom (Pure f) -> pure . f
      Atom (Effect f g) -> case f x of
          Perform (Const y) -> absurd y
          Request (Const y) -> absurd y


{-| An action in which a vendor handles a single request

    The action returns a 'Supply', which contains two things:

    - The response to the request
    - A new version of the vendor
-}

runVendor :: forall (action :: Action) (down :: Interface) (param :: Type) (product :: Type). Monad action =>
    Vendor NoInterface down action param -> down product
    -> param -> action (Supply NoInterface down action param product)

runVendor v r = runJob $ vendorToJob' v (Effect (Request r))


evalVendor :: forall (down :: Interface) (param :: Type) (product :: Type).
    Vendor NoInterface down NoAction param -> down product
    -> param -> Supply NoInterface down NoAction param product

evalVendor v r = evalJob $ vendorToJob' v (Effect (Request r))


-- | Makes requests, responds to requests, performs actions, and reads a parameter

newtype Vendor (up :: Interface) (down :: Interface) (action :: Action) (param :: Type) =
  Vendor
    { offer :: forall (product :: Type).
        down product -> Job up action param (Supply up down action param product) }


-- | The conclusion of a vendor's handling of a client request

data Supply (up :: Interface) (down :: Interface) (action :: Action) (param :: Type) (product :: Type) =
  Supply
    { supplyProduct :: product
        -- ^ The requested product
    , supplyNext :: Vendor up down action param
        -- ^ A new vendor to handle subsequent requests
    }

deriving stock instance Functor (Supply up down action param)


{-|

If @i@ is the downstream interface of vendor @a@ and the upstream interface
of job @b@, then we can form the composition @'vendorToJob' a b@.
When the 'Job' makes a request of type @i x@, the 'Vendor' replies with a
response of type @x@.

> ┌────────────────────────┐
> │   Vendor up i action   │
> └────────────────────────┘
>              ▲   │
>         i x  │   │  x
>              │   ▼
> ┌────────────────────────┐
> │  Job i action product  │
> └────────────────────────┘

-}

vendorToJob ::
    Vendor up down action param
    -> Job down action param product
    -> Job up action param product

vendorToJob up down =
    vendorToJob' up down <&> supplyProduct


vendorToVendor ::
    Vendor up middle action param
    -> Vendor middle down action param
    -> Vendor up down action param
vendorToVendor up down =
    Vendor \request -> vendorToJob' up (offer down request) <&> joinSupply


{-| Connect a vendor to a job, producing a job which
    returns both the product and a new version of the vendor.

    Use this function instead of 'vendorToJob' if you need to attach
    a succession of jobs to one stateful vendor.
-}
vendorToJob' ::
    forall
      (up :: Interface) (down :: Interface) (action :: Action) (param :: Type) (product :: Type).
    Vendor up down action param -> Job down action param product
    -> Job up action param (Supply up down action param product)

vendorToJob' up = \case
    Atom (Pure product) -> Pure (Supply product up)
    Atom (Effect (Perform action) f) -> Effect (Perform action) <&> (`Supply` up)
    Atom (Effect (Request request) f) -> offer up request
    Compose step1 step2 -> do
        Supply x up' <- vendorToJob' up step1
        let step2' = contraconstJob x step2
        vendorToJob' up' step2'


{-| Sort of resembles what a 'Control.Monad.join' implementation for
    'Supply' might look like, modulo a subtle difference in the types
-}

joinSupply ::
    Supply up middle action param (Supply middle down action param product)
    -> Supply up down action param product

joinSupply (Supply (Supply product nextDown) nextUp) =
    Supply product (vendorToVendor nextUp nextDown)


alterJob :: forall up up' action action' param product.
    (forall x. Effect up action x -> Job up' action' () x)
    -> Job up action param product -> Job up' action' param product

alterJob f = go
  where
    go :: forall x param'. Job up action param' x -> Job up' action' param' x
    go = \case
        Atom (Pure x) -> Atom (Pure x)
        Atom (Effect e f) -> contramapJob (\_ -> ()) (f e)
        Compose step1 step2 -> Compose (go step1) (go step2)

alterVendor :: forall up up' action action' down param.
    (forall x. Effect up action x -> Job up' action' () x)
    -> Vendor up down action param -> Vendor up' down action' param

alterVendor f = go
  where
    go :: Vendor up down action param -> Vendor up' down action' param
    go Vendor{ offer } = Vendor{ offer = fmap alterSupply . alterJob f . offer }

    alterSupply :: Supply up down action param product -> Supply up' down action' param product
    alterSupply s = s{ supplyNext = go (supplyNext s) }

-}
