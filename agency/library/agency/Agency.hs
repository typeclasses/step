module Agency (Client, Server, Serving (..), server, perform, request, Nil, nil, run, Connect ((>->))) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor

data Client up m a
  where
    Pure    :: a    -> Client up m a
    Perform :: m a  -> Client up m a
    Request :: up a -> Client up m a

    Bind :: Client up m x -> (x -> Client up m a) -> Client up m a

perform :: m a -> Client up m a
perform = Perform

request :: up a -> Client up m a
request = Request

newtype Server up down m =
    Server
      { serve :: Client up m (forall a.
            down a -> Client up m (Serving up down m a))
      }

data Serving up down m a = Serving{ next :: Server up down m, response :: a }

server = Server

instance Functor m => Functor (Client up m)
  where
    fmap f = \case
        Pure    x    ->  Pure $ f x
        Perform x    ->  Perform $ fmap f x
        Request x    ->  Request x `Bind` (Pure . f)
        Bind    x g  ->  x `Bind` fmap (fmap f) g

instance Functor m => Applicative (Client up m)
  where
    pure = Pure

    a1 <*> a2 = a1 `Bind` \f -> a2 <&> \x -> f x
    a1  *> a2 = a1 `Bind` \_ -> a2

instance Functor m => Monad (Client up m) where (>>=) = Bind

data Nil a

nil :: Nil a -> a
nil = \case{}

run :: forall up m a. Monad m => (forall r. up r -> m r) -> Client up m a -> m a
run z = go
  where
    go :: forall b. Monad m => Client up m b -> m b
    go = \case
      Pure    x    ->  pure x
      Perform x    ->  x
      Bind    x f  ->  go x >>= (go . f)
      Request x    ->  z x

connectPlus :: Functor m => Server a b m -> Client b m r -> Client a m (Serving a b m r)
connectPlus up = \case
    Pure    x    ->  Pure                    Serving{ next = up, response = x }
    Perform act  ->  Perform $ act <&> \x -> Serving{ next = up, response = x }
    Bind    x f  ->  connectPlus up x `Bind`
                       \Serving{ next = up', response = y } ->
                          connectPlus up' (f y)
    Request r    ->
        case serve up of
            Pure h      ->                                  h r
            Perform x   ->  Perform x          `Bind` \h -> h r
            Request r'  ->  Request r'         `Bind` \h -> h r
            Bind x f    ->  x `Bind` \y -> f y `Bind` \h -> h r

class Connect a b m downstream result | a b m downstream -> result where
    (>->) :: Server a b m -> downstream -> result

instance Functor m => Connect a b m (Client b m r) (Client a m r) where
    up >-> down = connectPlus up down <&> response

instance Functor m => Connect a b m (Server b c m) (Server a c m) where
    up >-> Server down =
        Server $
            connectPlus up down <&> \case
                Serving{ next = up', response = h } ->
                    \r ->
                        connectPlus up' (h r) <&>
                            \Serving{ next = up'', response = Serving{ next = down', response = s } } ->
                                Serving{ next = up'' >-> down', response = s }

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
