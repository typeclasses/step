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


-- | A pointed functor from any type constructor

data FreePointedFunctor f a =
    FreePure a | forall x. FreeMap (f x) (x -> a)

deriving stock instance Functor (FreePointedFunctor f)


-- | A chain of profunctor steps
--
-- Where `f` is a pointed functor, `Walk f ()` is a monad.
-- Thanks to its profunctorial nature, 'Compose' is easy to reassociate.

data Walk f a b = WalkStep (f b)
    | forall x. WalkCompose (a -> Walk f () x) (x -> Walk f () b)

pattern WalkPure :: b -> Walk (FreePointedFunctor f) a b
pattern WalkPure a = WalkStep (FreePure a)

pattern WalkMap :: f x -> (x -> b) -> Walk (FreePointedFunctor f) a b
pattern WalkMap x f = WalkStep (FreeMap x f)

{-# complete WalkPure, WalkMap, WalkCompose #-}

deriving stock instance Functor f => Functor (Walk f a)


newtype FreeWalk f a b = FreeWalk { unFreeWalk :: Walk (FreePointedFunctor f) a b }

deriving stock instance Functor (FreeWalk f a)

pattern FreeWalkPure :: b -> FreeWalk f a b
pattern FreeWalkPure x = FreeWalk (WalkPure x)

pattern FreeWalkMap :: f x -> (x -> b) -> FreeWalk f a b
pattern FreeWalkMap x f = FreeWalk (WalkMap x f)

pattern FreeWalkCompose ::
    (a -> FreeWalk f () x) -> (x -> FreeWalk f () b) -> FreeWalk f a b
pattern FreeWalkCompose a b <-
    FreeWalk (WalkCompose ((FreeWalk .) -> a) ((FreeWalk .) -> b))
  where
    FreeWalkCompose a b = FreeWalk (WalkCompose (unFreeWalk . a) (unFreeWalk . b))

{-# complete FreeWalkPure, FreeWalkMap, FreeWalkCompose #-}

runFreeWalk :: Monad m => (forall x. f x -> m x) -> FreeWalk f a b -> a -> m b
runFreeWalk f fw a = case fw of
    FreeWalkPure x -> pure x
    FreeWalkMap x g -> f x <&> g
    FreeWalkCompose step1 step2 ->
        case step1 a of
            FreeWalkCompose stepA stepB -> runFreeWalk f (FreeWalkCompose (stepA ()) (FreeWalkCompose stepB step2)) ()
        -- runFreeWalk f (step1 a) () >>= \x ->
        -- runFreeWalk f (step2 x) ()

-- | A monad from any type constructor

newtype FreeMonad f a =
    FreeMonad { unFreeMonad :: Walk (FreePointedFunctor f) () a }

pattern FreeMonadPure :: a -> FreeMonad f a
pattern FreeMonadPure x = FreeMonad (WalkPure x)

pattern FreeMonadMap :: f x -> (x -> b) -> FreeMonad f b
pattern FreeMonadMap x f = FreeMonad (WalkMap x f)

pattern FreeMonadCompose ::
    (() -> FreeMonad f x) -> (x -> FreeMonad f a) -> FreeMonad f a
pattern FreeMonadCompose a b <-
    FreeMonad (WalkCompose (((FreeMonad) .) -> a) (((FreeMonad) .) -> b))
  where
    FreeMonadCompose a b =
        FreeMonad (WalkCompose (unFreeMonad . a) (unFreeMonad . b))

{-# complete FreeMonadPure, FreeMonadMap, FreeMonadCompose #-}

pattern FreeMonadBind ::
    (FreeMonad f x) -> (x -> FreeMonad f a) -> FreeMonad f a
pattern FreeMonadBind a b <- FreeMonadCompose (($ ()) -> a) b
  where FreeMonadBind a b = FreeMonadCompose (\() -> a) b

{-# complete FreeMonadPure, FreeMonadMap, FreeMonadBind #-}

deriving stock instance Functor (FreeMonad f)

instance Applicative (FreeMonad f) where
    pure = FreeMonadPure
    (<*>) = Monad.ap

instance Monad (FreeMonad f) where
    (>>=) = FreeMonadBind

-- runFreeMonad :: Monad m => (forall x. f x -> m x) -> FreeMonad f a -> m a
-- runFreeMonad = _


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


-- | Monadic context that supports making requests and performing actions

newtype Job (up :: Interface) (action :: Action) (product :: Type) =
    Job { unJob :: FreeMonad (Effect up action) product }

pattern JobPure :: a -> Job up action a
pattern JobPure x = Job (FreeMonadPure x)

pattern JobRequest :: up x -> (x -> product) -> Job up action product
pattern JobRequest x f = Job (FreeMonadMap (Request x) f)

pattern JobPerform :: action x -> (x -> product) -> Job up action product
pattern JobPerform x f = Job (FreeMonadMap (Perform x) f)

pattern JobCompose ::
    (() -> Job up action x) -> (x -> Job up action a) -> Job up action a
pattern JobCompose a b <- Job (FreeMonadCompose ((Job .) -> a) ((Job .) -> b))
  where JobCompose a b = Job (FreeMonadCompose (unJob . a) (unJob . b))

{-# complete JobPure, JobRequest, JobPerform, JobCompose #-}

pattern JobBind ::
    (Job up action x) -> (x -> Job up action a) -> Job up action a
pattern JobBind a b <- JobCompose (($ ()) -> a) b
  where JobBind a b = JobCompose (\() -> a) b

{-# complete JobPure, JobRequest, JobPerform, JobBind #-}

deriving newtype instance Functor (Job up action)
deriving newtype instance Applicative (Job up action)
deriving newtype instance Monad (Job up action)


-- | An 'Interface' that admits no requests

type NoInterface = Const Void

type NoInterface :: Interface


-- | An 'Action' that admits no actions

type NoAction = Const Void

type NoAction :: Action

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
