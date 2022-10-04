module SupplyChain.Base
  (
    {- * Kinds   -} Interface, Action,
    {- * Client  -} Client, perform, order, run, eval,
    {- * Vendor  -} Vendor (..), Supply (..), (+>),
    {- * Connect -} Connect ((>->)), {- $connect -}
  )
  where

import SupplyChain.Core

import Control.Applicative (pure)
import Data.Function ((.))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)

-- | Use this instead of 'run' when @action = 'Identity'@
eval :: forall (up :: Interface) (product :: Type).
    (forall x. up x -> x) -> Client up Identity product -> product
eval f = runIdentity . run (pure . f)

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

-}
