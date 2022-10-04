{-| The innermost module of the supply-chain library

    It is recommended to instead use "SupplyChain.Base",
    which is slightly more abstract.
-}
module SupplyChain.Core where

import Control.Applicative (Applicative (pure, (*>), (<*>)))
import Control.Monad (Monad ((>>=)))
import Data.Function (($), (.), fix)
import Data.Functor (Functor (fmap), (<$>), (<&>))
import Data.Kind (Type)

{-| The kind of requests and responses exchanged between a vendor and a client

    If a client's upstream interface is @i@, then when the client makes a
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

data Client (up :: Interface) (action :: Action) (product :: Type) =
    Pure product
  | Perform (action product)
  | Request (up product)
  | forall x. Bind (Client up action x) (x -> Client up action product)

instance Functor action => Functor (Client up action)
  where
    fmap f = fix \r -> \case
        Pure product      ->  Pure (f product)
        Perform action    ->  Perform (fmap f action)
        Request request   ->  Bind (Request request) (Pure . f)
        Bind step1 step2  ->  Bind step1 (r . step2)

instance Functor action => Applicative (Client up action)
  where
    pure = Pure
    a1 <*> a2 = a1 `Bind` \f -> (f $) <$> a2
    a1 *> a2 = Bind a1 (\_ -> a2)

instance Functor action => Monad (Client up action)
  where
    (>>=) = Bind


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


-- | Makes requests, responds to requests, and performs actions

newtype Vendor (up :: Interface) (down :: Interface) (action :: Action) =
  Vendor
    { offer :: forall product.
        down product -> Client up action (Supply up down action product) }


-- | The conclusion of a vendor's handling of a client request

data Supply (up :: Interface) (down :: Interface) (action :: Action) (product :: Type) =
  (:->)
    { supplyProduct :: product
        -- ^ The requested product
    , supplyNext :: Vendor up down action
        -- ^ A new vendor to handle subsequent requests
    }

deriving stock instance Functor (Supply up down action)

toClient :: forall up down action product. Functor action =>
    Vendor up down action
    -> Client down action product
    -> Client up action (Supply up down action product)

toClient up = \case
    Pure product      ->  Pure $ product :-> up
    Perform action    ->  Perform (action <&> (:-> up))
    Request request   ->  offer up request
    Bind step1 step2  ->  (up `toClient` step1) `Bind` \supply ->
                            supplyNext supply `toClient` step2 (supplyProduct supply)

class Connect up down action client result
    | up client -> result
    , client -> down action
    , result -> up action
  where
    -- | Connects a vendor to a client (or to another vendor)
    (>->) :: Vendor up down action -> client -> result

instance Functor action =>
    Connect up down action (Client down action product) (Client up action product)
  where
    up >-> down =
        (up `toClient` down) <&> supplyProduct

instance Functor action =>
    Connect up middle action (Vendor middle down action) (Vendor up down action)
  where
    up >-> down =
        Vendor \request -> (up `toClient` offer down request) <&> joinSupply

joinSupply :: Functor action =>
    Supply up down1 action (Supply down1 down2 action product)
    -> Supply up down2 action product

joinSupply ((product :-> nextDown) :-> nextUp) =
    product :-> (nextUp >-> nextDown)
