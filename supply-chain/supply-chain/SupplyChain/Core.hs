{-| The innermost module of the supply-chain library

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

instance Functor (Client up action)
  where
    fmap f = fix \r -> \case
        Pure product      ->  Pure (f product)
        Perform action    ->  Bind (Perform action) (Pure . f)
        Request request   ->  Bind (Request request) (Pure . f)
        Bind step1 step2  ->  Bind step1 (r . step2)

instance Applicative (Client up action)
  where
    pure = Pure
    a1 <*> a2 = a1 `Bind` \f -> (f $) <$> a2
    a1 *> a2 = Bind a1 (\_ -> a2)

instance Monad (Client up action)
  where
    (>>=) = Bind


{-| Run a client in its 'Action' context

    The first argument is a handler that specifies what to do each
    time the client makes a request.
-}

runWith :: forall (up :: Interface) (action :: Action) (product :: Type). Monad action =>
    (forall x. up x -> action x) -> Client up action product -> action product

runWith handle = go
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


class Connect up down action client result
    | up client -> result
    , client -> down action
    , result -> up action
  where
    -- | Connects a vendor to a client (or to another vendor)
    (>->) :: Vendor up down action -> client -> result

instance Connect up down action (Client down action product) (Client up action product)
  where
    up >-> down =
        (up >+> down) <&> supplyProduct

instance Connect up middle action (Vendor middle down action) (Vendor up down action)
  where
    up >-> down =
        Vendor \request -> (up >+> offer down request) <&> joinSupply


{-| Connect a vendor to a client, producing a client which ultimately
    returns both the product and an updated vendor.

    This function is used to implement '(>->)'.
    Apart from that, perhaps it has no other use.
-}
(>+>) ::
    forall
      (up :: Interface) (down :: Interface) (action :: Action) (product :: Type).
    Vendor up down action -> Client down action product
    -> Client up action (Supply up down action product)

(>+>) up = \case
    Pure product      ->  Pure (product :-> up)
    Perform action    ->  Perform action <&> (:-> up)
    Request request   ->  offer up request
    Bind step1 step2  ->  (up >+> step1) `Bind` \supply ->
                            supplyNext supply >+> step2 (supplyProduct supply)


{-| Sort of resembles what a 'Control.Monad.join' implementation for
    'Supply' might look like, modulo a subtle difference in the types

    This function is used to implement '(>->)'.
    Apart from that, perhaps it has no other use.
-}

joinSupply ::
    Supply up middle action (Supply middle down action product)
    -> Supply up down action product

joinSupply ((product :-> nextDown) :-> nextUp) =
    product :-> (nextUp >-> nextDown)
