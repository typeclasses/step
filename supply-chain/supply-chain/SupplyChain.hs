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

    {- * Kinds -} Interface, Action,

    {- * Factory -} Factory, {- $factory -}
    {- ** How to create a factory -} {- $definingFactories -} order, perform,
    {- ** How to use a factory -} eval, run, evalWith, runWith,

    {- * Vendor -} Vendor (..), {- $vendor -} Supply (..),
    {- ** How to create a vendor -} {- $definingVendors -}
    {- ** Some simple vendors -} functionVendor, actionVendor, noVendor, map,

    {- * Connect (>->) -} Connect ((>->)), {- $connect -} (>+>),

    {- * Changing the Action context -} ActionFunctor (..), {- $actionMap -}

  )
  where

import SupplyChain.Core

import Control.Applicative (pure)
import Control.Monad (Monad)
import Data.Function ((.), ($))
import Data.Functor ((<&>))
import Data.Functor.Const (Const (getConst))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Void (absurd, Void)


-- | Perform an action in a factory's 'Action' context

perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Factory up action product

perform = Perform


-- | Send a request via the factory's upstream 'Interface'

order :: forall (up :: Interface) (action :: Action) (response :: Type).
    up response -> Factory up action response

order = Request


-- | Run a factory that makes no requests

run :: forall (action :: Action) (product :: Type). Monad action =>
    Factory (Const Void) action product -> action product

run = runWith (pure . absurd . getConst)


-- | Run a factory that makes no requests and performs no actions

eval :: forall (product :: Type).
    Factory (Const Void) Identity product -> product

eval = runIdentity . run


-- | Run a factory that performs no actions

evalWith :: forall (up :: Interface) (product :: Type).
    (forall x. up x -> x) -> Factory up Identity product -> product

evalWith f = runIdentity . runWith (pure . f)


-- | A simple stateless vendor that responds to each request by applying a pure function

functionVendor :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall response. down response -> response) -> Vendor up down action

functionVendor f = go
  where
    go = Vendor \x -> pure $ f x :-> go


-- | A simple stateless vendor that responds to each request by applying an effectful function

actionVendor :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall response. down response -> action response) -> Vendor up down action

actionVendor f = go
  where
    go = Vendor \x -> perform (f x) <&> (:-> go)


noVendor :: forall (up :: Interface) (action :: Action).
    Vendor up (Const Void) action
noVendor = Vendor \case{}


map :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall x. down x -> up x) -> Vendor up down action
map f = go
  where
    go = Vendor \x -> order (f x) <&> (:-> go)


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


{- $connect

If @i@ is the downstream interface of vendor @a@ and the upstream
interface of factory @b@, then we can form the composition @a '>->' b@.
When the factory makes a request of type @i x@, the vendor replies with a
response of type @x@.

> ┌────────────────────────────┐
> │     Vendor up i action     │
> └────────────────────────────┘
>              ▲   │
>         i x  │   │  x
>              │   ▼
> ┌────────────────────────────┐
> │  Factory i action product  │
> └────────────────────────────┘

The '(>->)' operation is associative; if @a@ and @b@ are vendors and @c@ is
a factory, then @(a >-> b) >-> c@ is the same supply chain as @a >-> (b >-> c)@.

Specializations:

@
('>->') :: 'Vendor' up down action   -> 'Factory' down action product -> 'Factory' up action product
('>->') :: 'Vendor' up middle action -> 'Vendor' middle down action   -> 'Vendor' up down action
@

-}


{- $actionMap

Specializations:

@
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Factory' up action1 product     -> 'Factory' up action2 product
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Vendor' up down action1         -> 'Vendor' up down action2
'actionMap' :: (forall x. action1 x -> action2 x) -> 'Supply' up down action1 product -> 'Supply' up down action2 product
@

-}
