module SupplyChain (Client, Vendor, Supply (..), vend, perform, request, Nil, nil, run, Connect ((>->))) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.Kind

type I = Type -> Type

data Client (a :: I) m r
  where
    Pure    :: r   -> Client a m r
    Perform :: m r -> Client a m r
    Request :: a r -> Client a m r

    Bind :: Client a m x -> (x -> Client a m r) -> Client a m r

perform :: m r -> Client a m r
perform = Perform

request :: a r -> Client a m r
request = Request

newtype Vendor (a :: I) (b :: I) m =
    Vendor
      { serve :: Client a m (forall r. b r -> Client a m (Supply a b m r))
      }

data Supply (a :: I) (b :: I) m r = Supply{ next :: Vendor a b m, product :: r }

vend = Vendor

instance Functor m => Functor (Client a m)
  where
    fmap f = \case
        Pure    x    ->  Pure $ f x
        Perform x    ->  Perform $ fmap f x
        Request x    ->  Request x `Bind` (Pure . f)
        Bind    x g  ->  x `Bind` fmap (fmap f) g

instance Functor m => Applicative (Client a m)
  where
    pure = Pure

    a1 <*> a2 = a1 `Bind` \f -> a2 <&> \x -> f x
    a1  *> a2 = a1 `Bind` \_ -> a2

instance Functor m => Monad (Client a m) where (>>=) = Bind

data Nil r

nil :: Nil r -> r
nil = \case{}

run :: forall a m r. Monad m => (forall x. a x -> m x) -> Client a m r -> m r
run z = go
  where
    go :: forall r'. Monad m => Client a m r' -> m r'
    go = \case
      Pure    x    ->  pure x
      Perform x    ->  x
      Bind    x f  ->  go x >>= (go . f)
      Request x    ->  z x

connectPlus :: Functor m => Vendor a b m -> Client b m r -> Client a m (Supply a b m r)
connectPlus up = \case
    Pure    x    ->  Pure                    Supply{ next = up, product = x }
    Perform act  ->  Perform $ act <&> \x -> Supply{ next = up, product = x }
    Bind    x f  ->  connectPlus up x `Bind`
                       \Supply{ next = up', product = y } ->
                          connectPlus up' (f y)
    Request r    ->
        case serve up of
            Pure h      ->                                  h r
            Perform x   ->  Perform x          `Bind` \h -> h r
            Request r'  ->  Request r'         `Bind` \h -> h r
            Bind x f    ->  x `Bind` \y -> f y `Bind` \h -> h r

class Connect a b m downstream result | a b m downstream -> result where
    (>->) :: Vendor a b m -> downstream -> result

instance Functor m => Connect a b m (Client b m r) (Client a m r) where
    up >-> down = connectPlus up down <&> product

instance Functor m => Connect a b m (Vendor b c m) (Vendor a c m) where
    up >-> Vendor down =
        Vendor $
            connectPlus up down <&> \case
                Supply{ next = up', product = h } ->
                    \r ->
                        connectPlus up' (h r) <&>
                            \Supply{ next = up'', product = Supply{ next = down', product = s } } ->
                                Supply{ next = up'' >-> down', product = s }

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
