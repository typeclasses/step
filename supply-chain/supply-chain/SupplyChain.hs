{- |

Description: The main API of the supply-chain library

A "supply chain" represents a flow of information from one 'Vendor' to the next,
and so on, ultimately reaching a 'Job' that returns a product.

@
(vendor1 '>->' vendor2 '>->' vendor3 '>->' job)
@

In the above example, @vendor2@ is said to be the /client/ which is immediately
/downstream/ of @vendor1@. A job or vendor can place an 'order', which is
fulfilled by the vendor /upstream/ of it. So, the orders made by the @job@
are served by @vendor3@, the orders made by @vendor3@ are served by @vendor2@,
and so on. If @vendor1@ does not make any requests and thus does not require yet
another vendor upstream of it, then the above expression can be converted into
an action using 'runJob'.

-}

module SupplyChain
  (

    {- * Kinds -}
    {- ** Interface -} Interface, NoInterface,
    {- ** Action -} Action, NoAction,

    {- * Job -}
    {- ** Type -} Job, {- $job -}
    {- ** How to create a job -} {- $definingJobs -} order, perform,
    {- ** How to use a job -} runJob, evalJob,

    {- * Vendor -}
    {- ** Type -} Vendor (..), {- $vendor -} Supply (..),
    {- ** How to create a vendor -} {- $definingVendors -}
    {- ** How to use a vendor -} {- $usingVendors -} runVendor, evalVendor,
    {- ** Some simple vendors -} functionVendor, actionVendor, noVendor, map,

    {- * Connection -}
    {- ** Functions -} vendorToJob, vendorToVendor,
    {- ** Polymorphically (>->) -} Connect (..),
    {- ** Reusing vendors -} vendorToJob',

    {- * Changing the Action context -} ActionFunctor (..), {- $actionMap -}

  )
  where

import SupplyChain.Core

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor ((<&>))
import Data.Kind (Type)


-- | Perform an action in a job's 'Action' context

perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Job up action product

perform = Perform


-- | Send a request via the job's upstream 'Interface'

order :: forall (up :: Interface) (action :: Action) (response :: Type).
    up response -> Job up action response

order = Request


-- | A simple stateless vendor that responds to each request by applying a pure function

functionVendor :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall response. down response -> response) -> Vendor up down action

functionVendor f = go
  where
    go = Vendor \x -> pure $ Supply (f x) go


-- | A simple stateless vendor that responds to each request by applying an effectful function

actionVendor :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall response. down response -> action response) -> Vendor up down action

actionVendor f = go
  where
    go = Vendor \x -> perform (f x) <&> (`Supply` go)


noVendor :: forall (up :: Interface) (action :: Action).
    Vendor up NoInterface action
noVendor = Vendor \case{}


map :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall x. down x -> up x) -> Vendor up down action
map f = go
  where
    go = Vendor \x -> order (f x) <&> (`Supply` go)


class Connect (up :: Interface) (down :: Interface)
    (action :: Action) (client :: Type) (result :: Type)
    | up client -> result
    , client -> down action
    , result -> up action
  where
    {-| Generalizes 'vendorToVendor' and 'vendorToJob'

        This operation is associative; if @a@ and @b@ are vendors and @c@ is a job,
        then @(a >-> b) >-> c@ is the same supply chain as @a >-> (b >-> c)@.
    -}
    (>->) :: Vendor up down action -> client -> result

instance Connect up down action
    (Job down action product)
    (Job up   action product)
  where
    (>->) = vendorToJob

instance Connect up middle action
    (Vendor middle down action)
    (Vendor up down action)
  where
    (>->) = vendorToVendor


{- $job

>              ▲   │
>        up x  │   │  x
>              │   ▼
> ┌─────────────────────────┐
> │  Job up action product  │
> └─────────────────────────┘

-}


{- $definingJobs

In addition to these functions for constructing jobs,
also keep in mind that 'Job' belongs to the 'Monad' class.

-}


{- $vendor

>              ▲   │
>        up x  │   │  x
>              │   ▼
> ┌───────────────────────────┐
> │   Vendor up down action   │
> └───────────────────────────┘
>              ▲   │
>      down y  │   │  y
>              │   ▼

-}


{- $definingVendors

We define vendors using the v'Vendor' constructor.
Please inspect its type carefully.

> forall product. down product -> Job up action (Supply up down action product)

A vendor is a function that accepts a request. The request type is
polymorphic but constrained by the vendor's downstream interface.

> forall product. down product -> Job up action (Supply up down action product)
>                 ^^^^^^^^^^^^

A vendor has an upstream interface and can do everything a job can,
therefore the request handler operates in a 'Job' context.

> forall product. down product -> Job up action (Supply up down action product)
>                                 ^^^^^^^^^^^^^^^^^

This allows the vendor to undertake a monadic sequence involving
'order' and 'perform' while fulfilling the request.

The final step in fulfilling a request is to return a 'Supply'.

> forall product. down product -> Job up action (Supply up down action product)
>                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A 'Supply' is written using its '(:->)' constructor, which has two parameters:

> (:->) :: product -> Vendor up down action -> Supply up down action product

The first is the vendor's response to the client's request.

> (:->) :: product -> Vendor up down action -> Supply up down action product
>          ^^^^^^^

The second is a new 'Vendor'.

> (:->) :: product -> Vendor up down action -> Supply up down action product
>                     ^^^^^^^^^^^^^^^^^^^^^

This latter component is what allows vendors to be stateful, and it is usually
defined recursively.

-}


{- $actionMap

Specializations:

@
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Job' up action1 product     -> 'FactJob action2 product
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Vendor' up down action1         -> 'Vendor' up down action2
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Supply' up down action1 product -> 'Supply' up down action2 product
@

-}

{- $usingVendors

The most common way to use a 'Vendor' is to connect it to a 'Job' using
'vendorToJob'. However, a vendor can also be used by itself with 'runVendor'.

-}
