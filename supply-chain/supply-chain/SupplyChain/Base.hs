{-| Minimal abstract API of the supply-chain library

This module aims to be more convenient than "SupplyChain.Core"
(a slightly lower-level API) and more stable than "SupplyChain.More"
(a wider collection of utilities with less clear inclusion criteria).
-}

module SupplyChain.Base
  (

    {- * Kinds   -}  Interface, Action,
    {- * Client  -}  Client,
    {- ** Introduction -}  perform, order,
    {- ** Elimination  -}  run, runWith, eval, evalWith,
    {- * Vendor  -}  Vendor (..), Supply (..),
    {- * Connect -}  Connect ((>->)), {- $connect -}

  )
  where

import SupplyChain.Core

import Control.Applicative (pure)
import Control.Monad (Monad)
import Data.Function ((.))
import Data.Functor.Const (Const (getConst))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Void (absurd, Void)


-- | Send a request via the client's upstream 'Interface'

perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Client up action product

perform = Perform


-- | Perform an action in a client's 'Action' context

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
