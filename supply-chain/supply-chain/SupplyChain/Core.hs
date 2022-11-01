{- |

Description: The innermost module of the supply-chain library

This module's chief virtue is minimalism. It aims to implement
only the most significant contributions of the library. If you
are new to supply-chain, start with "SupplyChain" instead,
which is better documented and somewhat more abstract.

-}
module SupplyChain.Core where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Arrow ((>>>))
import Control.Monad (Monad ((>>=)), (>=>))
import Data.Function (($), (.))
import Data.Functor (Functor (fmap), (<&>))
import Data.Kind (Type)
import Data.Functor.Const (Const (..))
import Data.Void (absurd, Void)

import qualified Control.Monad as Monad


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


data Effect (up :: Interface) (action :: Action) (product :: Type) =
    Request (up product)
  | Perform (action product)


-- | Monadic context that supports making requests, performing actions, and reading a parameter

data Job (up :: Interface) (action :: Action) (param :: Type) (product :: Type) =
    Pure product
  | Effect (Effect up action product)
  | Ask (param -> Job up action param product)
  | forall (x :: Type). Compose (Job up action param x) (Job up action x product)

instance Functor (Job up action param)
  where
    fmap f j = j `Compose` Ask \x -> Pure (f x)

instance Applicative (Job up action param)
  where
    pure = Pure
    (<*>) = Monad.ap

instance Monad (Job up action param)
  where
    step1 >>= step2 = Ask \x -> Compose step1 $ Ask \y -> contraconstJob x (step2 y)

-- todo: profunctor instance?

contramapJob :: (param' -> param) -> Job up action param product -> Job up action param' product
contramapJob f j = Compose (Ask \x -> Pure (f x)) j

contraconstJob :: param -> Job up action param product -> Job up action param' product
contraconstJob x = contramapJob (\_ -> x)


-- | An 'Interface' that admits no requests

type NoInterface = Const Void

type NoInterface :: Interface


-- | An 'Action' that admits no actions

type NoAction = Const Void

type NoAction :: Action


-- | Run a job in its 'Action' context

runJob :: forall (action :: Action) (param :: Type) (product :: Type). Monad action =>
    Job NoInterface action param product -> param -> action product

runJob = go
  where
    go :: forall a b. Job NoInterface action a b -> a -> action b
    go = \case
      -- If the root of the tree is left-associated, perform a rotation before proceeding.
      Compose (Compose step1 step2) step3 -> go (Compose step1 (Compose step2 step3))

      Pure product -> \_ -> pure product
      Compose step1 step2 -> go step1 >=> go step2
      Effect (Perform action) -> \_ -> action
      Effect (Request (Const x)) -> absurd x
      Ask f -> \x -> go (f x) x


-- | Run a job that performs no actions

evalJob :: forall (param :: Type) (product :: Type).
    Job NoInterface NoAction param product -> param -> product

evalJob =  go
  where
    go :: forall a b. Job NoInterface NoAction a b -> a -> b
    go = \case
      Compose (Compose step1 step2) step3 -> go (Compose step1 (Compose step2 step3))
      Pure product -> \_ -> product
      Compose step1 step2 -> go step1 >>> go step2
      Effect (Perform (Const x)) -> absurd x
      Effect (Request (Const x)) -> absurd x
      Ask f -> \x -> go (f x) x


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
    Pure product -> Pure (Supply product up)
    Effect (Perform action) -> Effect (Perform action) <&> (`Supply` up)
    Effect (Request request) -> offer up request
    Ask f -> Ask \x -> vendorToJob' up (f x)
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
        Pure x -> Pure x
        Compose step1 step2 -> Compose (go step1) (go step2)
        Effect e -> contramapJob (\_ -> ()) (f e)
        Ask g -> Ask (\x -> go (g x))

alterVendor :: forall up up' action action' down param.
    (forall x. Effect up action x -> Job up' action' () x)
    -> Vendor up down action param -> Vendor up' down action' param

alterVendor f = go
  where
    go :: Vendor up down action param -> Vendor up' down action' param
    go Vendor{ offer } = Vendor{ offer = fmap alterSupply . alterJob f . offer }

    alterSupply :: Supply up down action param product -> Supply up' down action' param product
    alterSupply s = s{ supplyNext = go (supplyNext s) }
