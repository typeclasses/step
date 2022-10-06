{- |

A "supply chain" represents a flow of information from one
'Vendor' to the next, and so on, ultimately reaching a 'Client'
who returns a product.

@
(vendor1 '>->' vendor2 '>->' vendor3 '>->' client)
@

In the above example, @vendor2@ is said to be /downstream/ of @vendor1@.
A client or vendor can place an 'order', which is fulfilled by the vendor
directly /upstream/ of it. So, the orders made by the @client@ are served by
@vendor3@, the orders made by @vendor3@ are served by @vendor2@, and so on.
-}

module SupplyChain
  (

    {- * Kinds -} Interface, Action,

    {- * Client -} Client,
    {- ** How to create a client -} {- $definingClients -} order, perform,
    {- ** How to use a client -} eval, run, evalWith, runWith,
    {- ** Miscellany -} hoistClient,

    {- * Vendor -} Vendor (..), Supply (..),
    {- ** How to create a vendor -} {- $definingVendors -}
    {- ** Some simple vendors -} functionVendor, actionVendor, noVendor, map,
    {- ** Miscellany -} hoistVendor,

    {- * Connect -} Connect ((>->)), {- $connect -}

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


-- | Perform an action in a client's 'Action' context

perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Client up action product

perform = Perform


-- | Send a request via the client's upstream 'Interface'

order :: forall (up :: Interface) (action :: Action) (response :: Type).
    up response -> Client up action response

order = Request


-- | Run a client that makes no requests

run :: forall (action :: Action) (product :: Type). Monad action =>
    Client (Const Void) action product -> action product

run = runWith (pure . absurd . getConst)


-- | Run a client that makes no requests and performs no actions

eval :: forall (product :: Type).
    Client (Const Void) Identity product -> product

eval = runIdentity . run


-- | Run a client that performs no actions

evalWith :: forall (up :: Interface) (product :: Type).
    (forall x. up x -> x) -> Client up Identity product -> product

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


noVendor :: Vendor up (Const Void) action
noVendor = Vendor \case{}


map :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall x. down x -> up x) -> Vendor up down action
map f = go
  where
    go = Vendor \x -> order (f x) <&> (:-> go)


hoistClient :: forall (up :: Interface) (a :: Action) (b :: Action) (product :: Type).
    (forall x. a x -> b x) -> Client up a product -> Client up b product

hoistClient f = go
  where
    go :: forall x. Client up a x -> Client up b x
    go = \case
        Pure x     ->  Pure x
        Request x  ->  Request x
        Perform x  ->  Perform (f x)
        Bind a b   ->  Bind (go a) (go . b)


hoistVendor :: forall (up :: Interface) (down :: Interface) (a :: Action) (b :: Action).
    (forall x. a x -> b x) -> Vendor up down a -> Vendor up down b

hoistVendor f = go
  where
    go (Vendor v) = Vendor \request ->
        hoistClient f (v request) <&> \(response :-> v') ->
            response :-> hoistVendor f v'


{- $definingClients

In addition to these functions for constructing clients,
also keep in mind that 'Client' belongs to the 'Monad' class.

-}


{- $definingVendors

We define vendors using the v'Vendor' constructor.
Please inspect its type carefully.

> forall product. down product -> Client up action (Supply up down action product)

A vendor is a function that accepts a request. The request type is
polymorphic but constrained by the vendor's downstream interface.

> forall product. down product -> Client up action (Supply up down action product)
>                 ^^^^^^^^^^^^

Since a vendor also has an upstream interface, vendors can act as
clients. The vendor therefore operates in a 'Client' context.

> forall product. down product -> Client up action (Supply up down action product)
>                                 ^^^^^^^^^^^^^^^^

This allows the vendor to undertake a monadic sequence involving
'order' and 'perform' while fulfilling the request.

The final step in fulfilling a request is to return a 'Supply'.

> forall product. down product -> Client up action (Supply up down action product)
>                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A 'Supply' is written using its '(:->)' constructor, which has two parameters:

> (:->) :: product -> Vendor up down action -> Supply up down action product

The first is the vendor's response to the downstream's request.

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
interface of client @b@, then we can form the composition @a '>->' b@.
When the client makes a request of type @i x@, the vendor replies with a
response of type @x@.

> ┌───────────────────────────┐
> │    Vendor up i action     │
> └───────────────────────────┘
>              ▲   │
>         i x  │   │  x
>              │   ▼
> ┌───────────────────────────┐
> │  Client i action product  │
> └───────────────────────────┘

The '(>->)' operation is associative; if @a@ and @b@ are vendors and @c@ is
a client, then @(a >-> b) >-> c@ is the same supply chain as @a >-> (b >-> c)@.

Specializations:

@
('>->') :: 'Vendor' up down action   -> 'Client' down action product -> 'Client' up action product
('>->') :: 'Vendor' up middle action -> 'Vendor' middle down action  -> 'Vendor' up down action
@

-}
