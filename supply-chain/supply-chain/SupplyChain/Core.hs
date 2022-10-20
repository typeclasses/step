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
import Data.Functor (Functor (fmap), (<$>), (<&>), void)
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Void (absurd, Void)


{-| The kind of requests and responses exchanged between a vendor and a factory

    If a factory's upstream interface is @i@, then when the factory makes a
    request of type @i x@, it receives a response of type @x@.

    Values of a type of this kind represent requests. Each constructor will
    typically have a constraint that specifies what type of response is
    expected in return. Types of this kind are therefore often
    <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html GADTs>.
-}

type Interface = Type -> Type


-- | A monadic context such as 'Data.Functor.Identity.Identity' or 'System.IO.IO'

type Action = Type -> Type


-- | Monadic context that supports making requests and performing actions

data Factory (up :: Interface) (action :: Action) (product :: Type) =
    Pure product
  | Perform (action product)
  | Request (up product)
  | forall (x :: Type). Bind (Factory up action x) (x -> Factory up action product)

instance Functor (Factory up action)
  where
    fmap f = fix \r -> \case
        Pure product      ->  Pure (f product)
        Perform action    ->  Bind (Perform action) (Pure . f)
        Request request   ->  Bind (Request request) (Pure . f)
        Bind step1 step2  ->  Bind step1 (r . step2)

instance Applicative (Factory up action)
  where
    pure = Pure
    a1 <*> a2 = a1 `Bind` \f -> (f $) <$> a2
    a1 *> a2 = Bind a1 (\_ -> a2)

instance Monad (Factory up action)
  where
    (>>=) = Bind


-- | An 'Interface' that admits no requests

type NoInterface = Const Void

type NoInterface :: Interface


-- | Run a factory in its 'Action' context

runFactory :: forall (action :: Action) (product :: Type). Monad action =>
    Factory NoInterface action product -> action product

runFactory = go
  where
    go :: forall x. Factory NoInterface action x -> action x
    go = \case
      Pure    product      ->  pure product
      Perform action       ->  action
      Bind    step1 step2  ->  go step1 >>= (go . step2)
      Request (Const x)    ->  absurd x


-- | An action in which a vendor handles a single request

runVendor :: forall (action :: Action) (down :: Interface) (receipt :: Type) (x :: Type). Monad action =>
    Vendor NoInterface down action receipt -> down x
    -> action (Supply NoInterface down action receipt x)

runVendor Vendor{ offer } r = runFactory (offer r)


-- | Makes requests, responds to requests, and performs actions

newtype Vendor (up :: Interface) (down :: Interface) (action :: Action) (receipt :: Type) =
  Vendor
    { offer :: forall (product :: Type).
        down product -> Factory up action (Supply up down action receipt product) }

instance Functor (Vendor up down action)
  where
    fmap f Vendor{ offer } = Vendor{ offer = \request -> fmap (supplyReceiptMap f) (offer request) }


supplyReceiptMap :: (receipt1 -> receipt2)
    -> Supply up down action receipt1 product
    -> Supply up down action receipt2 product

supplyReceiptMap f (Supply a b c) = Supply a (f b) (fmap f c)


-- | The conclusion of a vendor's handling of a client request

data Supply (up :: Interface) (down :: Interface) (action :: Action) (receipt :: Type) (product :: Type) =
  Supply
    { supplyProduct :: product
        -- ^ The requested product
    , supplyReceipt :: receipt
    , supplyNext :: Vendor up down action receipt
        -- ^ A new vendor to handle subsequent requests
    }

deriving stock instance Functor (Supply up down action receipt)


{-|

If @i@ is the downstream interface of vendor @a@ and the upstream interface
of factory @b@, then we can form the composition @'vendorToFactory' a b@.
When the 'Factory' makes a request of type @i x@, the 'Vendor' replies with a
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

-}

vendorToFactory ::
    Vendor up down action ()
    -> Factory down action product
    -> Factory up action product

vendorToFactory up down =
    vendorToFactory' up down <&> supplyProduct


vendorToVendor ::
    Vendor up middle action ()
    -> Vendor middle down action receipt
    -> Vendor up down action receipt
vendorToVendor up down =
    Vendor \request -> vendorToFactory' up (offer down request) <&> joinSupply


{-| Connect a vendor to a factory, producing a factory which
    returns both the product and a new version of the vendor.

    Use this function instead of 'vendorToFactory' if you need to attach
    a succession of factories to one stateful vendor.
-}
vendorToFactory' ::
    forall
      (up :: Interface) (down :: Interface) (action :: Action) (product :: Type).
    Vendor up down action () -> Factory down action product
    -> Factory up action (Supply up down action () product)

vendorToFactory' up = \case
    Pure product      ->  Pure (Supply product () up)
    Perform action    ->  Perform action <&> \product -> Supply product () up
    Request request   ->  offer up request
    Bind step1 step2  ->  (vendorToFactory' up step1) `Bind` \supply ->
                            vendorToFactory' (supplyNext supply)
                              (step2 (supplyProduct supply))


{-| Sort of resembles what a 'Control.Monad.join' implementation for
    'Supply' might look like, modulo a subtle difference in the types
-}

joinSupply ::
    Supply up middle action () (Supply middle down action receipt product)
    -> Supply up down action receipt product

joinSupply (Supply (Supply product receipt nextDown) _receipt nextUp) =
    Supply product receipt (vendorToVendor nextUp nextDown)


class ActionFunctor (action1 :: Action) (action2 :: Action) (x1 :: Type) (x2 :: Type)
    | x1 action1 x2 -> action2
    , x1 x2 action2 -> action1
    , x1 action1 action2 -> x2
    , x2 action1 action2 -> x1
  where
    -- | Changes the 'Action' context
    actionMap :: (forall (x :: Type). action1 x -> action2 x) -> x1 -> x2

instance ActionFunctor action1 action2
    (Factory up action1 product)
    (Factory up action2 product)
  where
    actionMap f = go
      where
        go :: forall (x :: Type). Factory up action1 x -> Factory up action2 x
        go = \case
            Pure x     ->  Pure x
            Request x  ->  Request x
            Perform x  ->  Perform (f x)
            Bind a b   ->  Bind (go a) (go . b)

instance ActionFunctor action1 action2
    (Vendor up down action1 receipt)
    (Vendor up down action2 receipt)
  where
    actionMap f = go
      where
        go (Vendor v) = Vendor \request ->
            actionMap f (v request) <&> \(Supply response receipt v') ->
                Supply response receipt (go v')

instance ActionFunctor action1 action2
    (Supply up down action1 receipt product)
    (Supply up down action2 receipt product)
  where
    actionMap f (Supply x r v) = Supply x r (actionMap f v)
