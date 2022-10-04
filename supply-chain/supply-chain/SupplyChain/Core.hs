{-| The innermost module of the supply-chain library

    It is recommended to instead use "SupplyChain.Base",
    which exports the types abstractly.
-}
module SupplyChain.Core where

import Control.Applicative (Applicative (pure, (*>), (<*>)), (<$>))
import Control.Monad (Monad ((>>=)), Functor (fmap))
import Data.Function (($), (.), fix)
import Data.Functor (Functor (fmap), (<$>), (<&>))
import Data.Kind (Type)

{-| The kind of requests and responses exchanged between a vendor and a client

    If @i@ is the downstream interface of vendor @a@ and the upstream
    interface of client @b@, then we can form the composition @a '>->' b@.
    When the client makes a request of type @i x@, the vendor replies with a
    response of type @x@.

    Values of a type of this kind represent requests. Each constructors will
    typically have a constraint that specifies what type of response is
    expected in return. Types of this kind are therefore often
    <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html GADTs>.
-}

type Interface = Type -> Type


-- | A monadic context such as 'Data.Functor.Identity.Identity' or 'System.IO.IO'

type Action = Type -> Type


-- | Monadic context that supports making requests and performing actions

data Client (up :: Interface) (action :: Action) (product :: Type)
  where
    Pure    ::        product -> Client up action product
    Perform :: action product -> Client up action product
    Request :: up     product -> Client up action product

    Bind :: Client up action x
         -> (x -> Client up action product)
         ->       Client up action product

instance Functor action => Functor (Client up action)
    where { fmap = mapClient }

instance Functor action => Applicative (Client up action)
    where { pure = Pure; (<*>) = apClient; (*>) = apClient' }

instance Functor action => Monad (Client up action)
    where { (>>=) = Bind }


-- | Send a request via the client's upstream 'Interface'

perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Client up action product

perform = Perform


-- | Perform an action in a client's 'Action' context

order :: forall (up :: Interface) (action :: Action) (response :: Type).
    up response -> Client up action response

order = Request


{-| Run a client in its 'Action' context

    The first argument is a handler that specifies what to do each
    time the client makes a request.
-}

run :: forall (up :: Interface) (action :: Action) (product :: Type). Monad action =>
    (forall x. up x -> action x) -> Client up action product -> action product

run handle = go
  where
    go :: forall x. Client up action x -> action x
    go = \case
      Pure    product      ->  pure product
      Perform action       ->  action
      Bind    step1 step2  ->  go step1 >>= (go . step2)
      Request request      ->  handle request


-- | 'fmap' for 'Client'

mapClient :: forall a b up action. Functor action =>
    (a -> b) -> Client up action a -> Client up action b

mapClient f = fix \r -> \case
    Pure product      ->  Pure (f product)
    Perform action    ->  Perform (fmap f action)
    Request request   ->  Bind (Request request) (Pure . f)
    Bind step1 step2  ->  Bind step1 (r . step2)


-- | '(<*>)' for 'Client'

apClient :: forall a b up action. Functor action =>
    Client up action (a -> b) -> Client up action a -> Client up action b
apClient a1 a2 = a1 `Bind` \f -> (f $) <$> a2


-- | '(*>)' for 'Client'

apClient' :: forall a b up action.
    Client up action a -> Client up action b -> Client up action b
apClient' a1 a2 = Bind a1 (\_ -> a2)


-- | Makes requests, responds to requests, and performs actions

newtype Vendor (up :: Interface) (down :: Interface) (action :: Action) =
  Vendor
    { runVendor ::
        Client up action
          ( forall product.
              down product -> Client up action (Supply up down action product)
          )
    }


data Supply (up :: Interface) (down :: Interface) (action :: Action) (product :: Type) =
  Supply
    { supplyNext :: Vendor up down action
    , supplyProduct :: product
    }

deriving stock instance Functor (Supply up down action)


connectVendorToClient :: forall up down action product. Functor action =>
    Vendor up down action -> Client down action product -> Client up action (Supply up down action product)

connectVendorToClient vendor =
  \case
    Pure product      ->  Pure Supply{ supplyNext = vendor, supplyProduct = product }
    Perform action    ->  Perform (action <&> \product -> Supply{ supplyNext = vendor, supplyProduct = product })
    Request request   ->  connectVendorToRequest vendor request
    Bind step1 step2  ->  connectVendorToClient vendor step1 `Bind` \supply ->
                            connectVendorToClient (supplyNext supply) (step2 (supplyProduct supply))

connectVendorToRequest :: forall up down action product.
    Vendor up down action -> down product -> Client up action (Supply up down action product)

connectVendorToRequest up =
  case runVendor up of
    Pure handle       ->  \request ->                                              handle request
    Perform action    ->  \request -> Perform action             `Bind` \handle -> handle request
    Request request'  ->  \request -> Request request'           `Bind` \handle -> handle request
    Bind step1 step2  ->  \request -> step1 `Bind` \x -> step2 x `Bind` \handle -> handle request


connectVendorToVendor :: forall up middle down action. Functor action =>
    Vendor up middle action -> Vendor middle down action -> Vendor up down action

connectVendorToVendor up (Vendor down) =
  Vendor $
    connectVendorToClient up down <&> \supply request ->
      connectVendorToClient (supplyNext supply) (supplyProduct supply request) <&>
        supplyJoin


supplyJoin :: forall up middle down action product. Functor action =>
    Supply up middle action (Supply middle down action product) -> Supply up down action product

supplyJoin s =
  Supply
    { supplyNext = connectVendorToVendor (supplyNext s) (supplyNext (supplyProduct s))
    , supplyProduct = supplyProduct (supplyProduct s)
    }

class Connect a b m downstream result | a b m downstream -> result where
    (>->) :: Vendor a b m -> downstream -> result

instance Functor m => Connect a b m (Client b m r) (Client a m r) where
    up >-> down = connectVendorToClient up down <&> supplyProduct

instance Functor m => Connect a b m (Vendor b c m) (Vendor a c m) where
    (>->) = connectVendorToVendor
