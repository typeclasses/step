{- |

Description: The main API of the supply-chain library

A "supply chain" represents a flow of information from one
'Vendor' to the next, and so on, ultimately reaching a 'Factory'
who returns a product.

@
(vendor1 '>->' vendor2 '>->' vendor3 '>->' factory)
@

In the above example, @vendor2@ is said to be the /client/ which is immediately
/downstream/ of @vendor1@. A factory or vendor can place an 'order', which is
fulfilled by the vendor /upstream/ of it. So, the orders made by the @factory@
are served by @vendor3@, the orders made by @vendor3@ are served by @vendor2@,
and so on.

-}

module SupplyChain
  (

    {- * Kinds -}
    {- ** Interface -} Interface, NoInterface,
    {- ** Action -} Action, NoAction,

    {- * Factory -}
    {- ** Type -} Factory, {- $factory -}
    {- ** How to create a factory -} {- $definingFactories -} order, perform,
    {- ** How to use a factory -} runFactory, evalFactory,

    {- * Vendor -}
    {- ** Type -} Vendor (..), {- $vendor -} Supply (..),
    {- ** How to create a vendor -} {- $definingVendors -}
    {- ** How to use a vendor -} {- $usingVendors -} runVendor, evalVendor,
    {- ** Some simple vendors -} functionVendor, actionVendor, noVendor, map,

    {- * Connection -}
    {- ** Functions -} vendorToFactory, vendorToVendor,
    {- ** Polymorphically (>->) -} Connect (..),
    {- ** Reusing vendors -} vendorToFactory',

    {- * Changing the Action context -} ActionFunctor (..), {- $actionMap -}

  )
  where

import SupplyChain.Core

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)


-- | An 'Action' that permits no action

type NoAction = Identity

type NoAction :: Action


-- | Perform an action in a factory's 'Action' context

perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Factory up action product

perform = Perform


-- | Send a request via the factory's upstream 'Interface'

order :: forall (up :: Interface) (action :: Action) (response :: Type).
    up response -> Factory up action response

order = Request


-- | Run a factory that performs no actions

evalFactory :: forall (product :: Type).
    Factory NoInterface NoAction product -> product

evalFactory f = runIdentity (runFactory f)


evalVendor :: forall (down :: Interface) (x :: Type).
    Vendor NoInterface down NoAction -> down x -> Supply NoInterface down NoAction x

evalVendor v r = runIdentity (runVendor v r)


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
    {-| Generalizes 'vendorToVendor' and 'vendorToFactory'

        This operation is associative; if @a@ and @b@ are vendors and @c@ is a factory,
        then @(a >-> b) >-> c@ is the same supply chain as @a >-> (b >-> c)@.
    -}
    (>->) :: Vendor up down action -> client -> result

instance Connect up down action
    (Factory down action product)
    (Factory up   action product)
  where
    (>->) = vendorToFactory

instance Connect up middle action
    (Vendor middle down action)
    (Vendor up down action)
  where
    (>->) = vendorToVendor


{- $factory

>              ▲   │
>        up x  │   │  x
>              │   ▼
> ┌─────────────────────────────┐
> │  Factory up action product  │
> └─────────────────────────────┘

-}


{- $definingFactories

In addition to these functions for constructing factories,
also keep in mind that 'Factory' belongs to the 'Monad' class.

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

> forall product. down product -> Factory up action (Supply up down action product)

A vendor is a function that accepts a request. The request type is
polymorphic but constrained by the vendor's downstream interface.

> forall product. down product -> Factory up action (Supply up down action product)
>                 ^^^^^^^^^^^^

A vendor has an upstream interface and can do everything a factory can,
therefore the request handler operates in a 'Factory' context.

> forall product. down product -> Factory up action (Supply up down action product)
>                                 ^^^^^^^^^^^^^^^^^

This allows the vendor to undertake a monadic sequence involving
'order' and 'perform' while fulfilling the request.

The final step in fulfilling a request is to return a 'Supply'.

> forall product. down product -> Factory up action (Supply up down action product)
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
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Factory' up action1 product     -> 'Factory' up action2 product
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Vendor' up down action1         -> 'Vendor' up down action2
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Supply' up down action1 product -> 'Supply' up down action2 product
@

-}

{- $usingVendors

The most common way to use a 'Vendor' is to connect it to a 'Factory' using
'vendorToFactory'. However, a vendor can also be used by itself with 'runVendor'.

-}
