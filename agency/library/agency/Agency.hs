module Agency where

import Relude hiding (Proxy)

data Client up m a
  where
    Pure    :: a    -> Client up m a
    Action  :: m a  -> Client up m a
    Request :: up a -> Client up m a
    Bind    :: Client up m x -> (x -> Client up m a) -> Client up m a

newtype Handler up down m =
    Handler{ handle :: forall a. down a -> Client up m (Server up down m, a) }

newtype Server up down m =
    Server{ serve :: Client up m (Handler up down m) }

instance Functor m => Functor (Client up m)
  where
    fmap f = \case
        Pure    x   -> Pure $ f x
        Action  x   -> Action $ fmap f x
        Request x   -> Request x `Bind` (Pure . f)
        Bind    x g -> x `Bind` fmap (fmap f) g

instance Functor m => Applicative (Client up m)
  where
    pure = Pure
    a1 <*> a2 = a1 `Bind` \f -> a2 <&> \x -> f x
    a1  *> a2 = a1 `Bind` \_ -> a2

instance Functor m => Monad (Client up m) where (>>=) = Bind

data Nil a

run :: Monad m => Client Nil m a -> m a
run = \case
    Pure   x   -> pure x
    Action x   -> x
    Bind   x f -> run x >>= (run . f)

connectServerToClient :: Functor m => Server up x m -> Client x m a -> Client up m (Server up x m, a)
connectServerToClient up down =
    case down of
        Pure    x   -> Pure (up, x)
        Action  x   -> Action (fmap (up,) x)
        Bind    x f -> connectServerToClient up x `Bind` \(up', y) -> connectServerToClient up' (f y)
        Request r   ->
            case serve up of
                Pure h     ->                                 handle h r
                Action x   -> Action x           `Bind` \h -> handle h r
                Request r' -> Request r'         `Bind` \h -> handle h r
                Bind x f   -> x `Bind` \y -> f y `Bind` \h -> handle h r

connectServerToServer :: Functor m => Server up x m -> Server x down m -> Server up down m
connectServerToServer up (Server down) =
    Server (connectServerToClient up down <&> \(up', h) -> connectServerToHandler up' h)

connectServerToHandler :: Functor m => Server up x m -> Handler x down m -> Handler up down m
connectServerToHandler up (Handler h) = Handler \r -> _ (h r)

-- (>->) :: Functor m => Server am b m -> Agent ('Just b) cm m r -> Agent am cm m r
-- a >-> b = fmap snd (connect a b)

-- relaxAgentDown :: Functor m => Agent am 'Nothing m r -> Agent am cm m r
-- relaxAgentDown = r
--   where
--     r :: Functor m => Agent am 'Nothing m r -> Agent am cm m r
--     r = \case
--       AgentRequest x -> AgentRequest x
--       AgentAction x -> AgentAction x
--       AgentBind x f -> AgentBind (r x) (fmap r f)
--       AgentPure x -> AgentPure x

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
