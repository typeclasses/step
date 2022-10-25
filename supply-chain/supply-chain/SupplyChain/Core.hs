{- |

Description: The innermost module of the supply-chain library

This module's chief virtue is minimalism. It aims to implement
only the most significant contributions of the library. If you
are new to supply-chain, start with "SupplyChain" instead,
which is better documented and somewhat more abstract.

-}
module SupplyChain.Core where

import Control.Applicative (Applicative (pure, (*>), (<*>)))
import Control.Monad (Monad ((>>=)))
import Data.Function (($), (.), fix)
import Data.Functor (Functor (fmap), (<$>), (<&>))
import Data.Kind (Type)
import Data.Functor.Const (Const (..))
import Data.Void (absurd, Void)


{-| The kind of requests and responses exchanged between a vendor and a job

    If a job's upstream interface is @i@, then when the job makes a
    request of type @i x@, it receives a response of type @x@.

    Values of a type of this kind represent requests. Each constructor will
    typically have a constraint that specifies what type of response is
    expected in return. Types of this kind are therefore often
    <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html GADTs>.

    The lack of any interface at all can be expressed as 'NoInterface'.
-}

type Interface = Type -> Type


{-| A monadic context such as 'System.IO.IO'

    The lack of any actions at all can be expressed as 'NoAction'.
-}

type Action = Type -> Type


-- | Monadic context that supports making requests and performing actions

data Job (up :: Interface) (action :: Action) (product :: Type) =
    Pure product
  | Perform (action product)
  | Request (up product)
  | forall (x :: Type). Bind (Job up action x) (x -> Job up action product)

instance Functor (Job up action)
  where
    fmap f = fix \r -> \case
        Pure product      ->  Pure (f product)
        Perform action    ->  Bind (Perform action) (Pure . f)
        Request request   ->  Bind (Request request) (Pure . f)
        Bind step1 step2  ->  Bind step1 (r . step2)

instance Applicative (Job up action)
  where
    pure = Pure
    a1 <*> a2 = a1 `Bind` \f -> (f $) <$> a2
    a1 *> a2 = Bind a1 (\_ -> a2)

instance Monad (Job up action)
  where
    (>>=) = Bind


-- | An 'Interface' that admits no requests

type NoInterface = Const Void

type NoInterface :: Interface


-- | An 'Action' that admits no actions

type NoAction = Const Void

type NoAction :: Action


-- | Run a job in its 'Action' context

runJob :: forall (action :: Action) (product :: Type). Monad action =>
    Job NoInterface action product -> action product

runJob = go
  where
    go :: forall x. Job NoInterface action x -> action x
    go = \case
      Pure    product      ->  pure product
      Bind    step1 step2  ->  go step1 >>= (go . step2)
      Perform action       ->  action
      Request (Const x)    ->  absurd x


-- | Run a job that performs no actions

evalJob :: forall (product :: Type).
    Job NoInterface NoAction product -> product

evalJob =  go
  where
    go :: forall x. Job NoInterface NoAction x -> x
    go = \case
      Pure    product      ->  product
      Bind    step1 step2  ->  go (step2 (go step1))
      Perform (Const x)    ->  absurd x
      Request (Const x)    ->  absurd x


{-| An action in which a vendor handles a single request

    The action returns a 'Supply', which contains two things:

    - The response to the request
    - A new version of the vendor
-}

runVendor :: forall (action :: Action) (down :: Interface) (x :: Type). Monad action =>
    Vendor NoInterface down action -> down x -> action (Supply NoInterface down action x)

runVendor v r = runJob $ vendorToJob' v (Request r)


evalVendor :: forall (down :: Interface) (x :: Type).
    Vendor NoInterface down NoAction -> down x -> Supply NoInterface down NoAction x

evalVendor v r = evalJob $ vendorToJob' v (Request r)


-- | Makes requests, responds to requests, and performs actions

newtype Vendor (up :: Interface) (down :: Interface) (action :: Action) =
  Vendor
    { offer :: forall (product :: Type).
        down product -> Job up action (Supply up down action product) }


-- | The conclusion of a vendor's handling of a client request

data Supply (up :: Interface) (down :: Interface) (action :: Action) (product :: Type) =
  Supply
    { supplyProduct :: product
        -- ^ The requested product
    , supplyNext :: Vendor up down action
        -- ^ A new vendor to handle subsequent requests
    }

deriving stock instance Functor (Supply up down action)


{-|

If @i@ is the downstream interface of vendor @a@ and the upstream interface
of job @b@, then we can form the composition @'vendorToJob' a b@.
When the 'Job' makes a request of type @i x@, the 'Vendor' replies with a
response of type @x@.

> ┌────────────────────────────┐
> │     Vendor up i action     │
> └────────────────────────────┘
>              ▲   │
>         i x  │   │  x
>              │   ▼
> ┌────────────────────────┐
> │  Job i action product  │
> └────────────────────────┘

-}

vendorToJob ::
    Vendor up down action
    -> Job down action product
    -> Job up action product

vendorToJob up down =
    vendorToJob' up down <&> supplyProduct


vendorToVendor ::
    Vendor up middle action
    -> Vendor middle down action
    -> Vendor up down action
vendorToVendor up down =
    Vendor \request -> vendorToJob' up (offer down request) <&> joinSupply


{-| Connect a vendor to a job, producing a job which
    returns both the product and a new version of the vendor.

    Use this function instead of 'vendorToJob' if you need to attach
    a succession of jobs to one stateful vendor.
-}
vendorToJob' ::
    forall
      (up :: Interface) (down :: Interface) (action :: Action) (product :: Type).
    Vendor up down action -> Job down action product
    -> Job up action (Supply up down action product)

vendorToJob' up = \case
    Pure product      ->  Pure (Supply product up)
    Perform action    ->  Perform action <&> (`Supply` up)
    Request request   ->  offer up request
    Bind step1 step2  ->  (vendorToJob' up step1) `Bind` \supply ->
                            vendorToJob' (supplyNext supply)
                              (step2 (supplyProduct supply))


{-| Sort of resembles what a 'Control.Monad.join' implementation for
    'Supply' might look like, modulo a subtle difference in the types
-}

joinSupply ::
    Supply up middle action (Supply middle down action product)
    -> Supply up down action product

joinSupply (Supply (Supply product nextDown) nextUp) =
    Supply product (vendorToVendor nextUp nextDown)


class ActionFunctor (action1 :: Action) (action2 :: Action) (x1 :: Type) (x2 :: Type)
    | x1 action1 x2 -> action2
    , x1 x2 action2 -> action1
    , x1 action1 action2 -> x2
    , x2 action1 action2 -> x1
  where
    -- | Changes the 'Action' context
    actionMap :: (forall (x :: Type). action1 x -> action2 x) -> x1 -> x2

instance ActionFunctor action1 action2
    (Job up action1 product)
    (Job up action2 product)
  where
    actionMap f = go
      where
        go :: forall (x :: Type). Job up action1 x -> Job up action2 x
        go = \case
            Pure x     ->  Pure x
            Request x  ->  Request x
            Perform x  ->  Perform (f x)
            Bind a b   ->  Bind (go a) (go . b)

instance ActionFunctor action1 action2
    (Vendor up down action1)
    (Vendor up down action2)
  where
    actionMap f = go
      where
        go (Vendor v) = Vendor \request ->
            actionMap f (v request) <&> \(Supply response v') ->
                Supply response (go v')

instance ActionFunctor action1 action2
    (Supply up down action1 product)
    (Supply up down action2 product)
  where
    actionMap f (Supply x v) = Supply x (actionMap f v)
