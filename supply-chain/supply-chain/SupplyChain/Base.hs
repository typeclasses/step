{-| Minimal abstract API of the supply-chain library

Additional utilities may be found in "SupplyChain.More".

For a slightly lower-level API, see "SupplyChain.Core".
-}

module SupplyChain.Base
  (

    {- * Kinds   -}  Interface, Action,
    {- * Client  -}  Client, perform, order, run,
    {- * Vendor  -}  Vendor (..), Supply (..),
    {- * Connect -}  Connect ((>->)), {- $connect -}

  )
  where

import SupplyChain.Core

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
