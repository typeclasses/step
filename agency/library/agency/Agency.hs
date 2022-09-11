module Agency where

import Relude hiding (Proxy)

type Response = Type -> Type

type Context = Type -> Type

data Agent (am :: Maybe Response) (bm :: Maybe Response) (m :: Context) r where

    -- | Request of type @a x@ + continuation from response type @x@
    AgentRequest :: Client x a bm m r -> Agent ('Just a) bm m r

    -- | Continuation from request type @b x@ producing a response of type @x@
    AgentServe :: Server am b m r -> Agent am ('Just b) m r

    -- | Action lifted from the base monad @m@
    AgentAction :: m (Agent am bm m r) -> Agent am bm m r

    -- | ('>>=')
    AgentBind :: Agent am bm m x -> (x -> Agent am bm m r) -> Agent am bm m r

    -- | 'pure'
    AgentPure :: r -> Agent am bm m r

data Client x (a :: Response) (bm :: Maybe Response) (m :: Context) r = Client
    { clientRequest :: a x
        -- ^ A client makes a request of type @a x@
    , clientContinuation :: x -> Agent ('Just a) bm m r
        -- ^ A client receives a response of type @x@ from upstream
    }

newtype Server (am :: Maybe Response) (b :: Response) (m :: Context) r = Server
    { serverHandler :: forall x. b x -> Reaction x am b m r
        -- ^ A server receives a @b x@ request from downstream and responds with @x@
    }

newtype Reaction x (am :: Maybe Response) (b :: Response) (m :: Context) r = Reaction
    { reactionAgent :: Agent am 'Nothing m (Yield x am b m r)
        -- ^ A server in the process of reacting to a request
    }

data Yield x (am :: Maybe Response) (b :: Response) (m :: Context) r = Yield
    { yieldResponse :: x
        -- ^ Response returned by a server
    , yieldContinuation :: Agent am ('Just b) m r
        -- ^ What's next for the server
    }

newtype Daemon am b m = Daemon{ daemonAgent :: Agent am ('Just b) m Void }

deriving stock instance Functor m => Functor (Client x a bm m)
deriving stock instance Functor m => Functor (Server am b m)
deriving stock instance Functor m => Functor (Yield x am b m)
deriving stock instance Functor m => Functor (Agent am bm m)
deriving stock instance Functor m => Functor (Reaction x am b m)

instance Functor m => Applicative (Agent am bm m) where
    pure = AgentPure
    a1 <*> a2 = AgentBind a1 \f -> a2 <&> \x -> f x
    a1 *> a2 = AgentBind a1 \_ -> a2

instance Functor m => Monad (Agent am bm m) where
    (>>=) = AgentBind

runAgent :: Monad m => Agent 'Nothing 'Nothing m r -> m r
runAgent = \case
    AgentPure x -> pure x
    AgentAction x -> x >>= runAgent
    AgentBind x f -> runAgent x >>= (runAgent . f)

(>->) :: forall am b cm m r. Functor m =>
    Daemon am b m -> Agent ('Just b) cm m r -> Agent am cm m r
_ >-> AgentPure x = AgentPure x
up >-> AgentAction x = AgentAction (fmap (up >->) x)
up >-> AgentServe (Server f) = AgentServe $ Server \x -> up +>> f x

(+>>) :: Functor m =>
    Daemon am b m -> Reaction x ('Just b) c m r -> Reaction x am c m r
up +>> Reaction (AgentPure (Yield y down)) = Reaction $ pure $ Yield y $ up >-> down
up +>> Reaction (AgentAction a) = Reaction $ AgentAction $ a <&> \a' ->
    reactionAgent $ up +>> Reaction a'
Daemon (AgentServe (Server f)) +>> Reaction (AgentRequest (Client x g)) = Reaction do
    Yield y up' <- reactionAgent $ f x
    reactionAgent $ Daemon up' +>> Reaction (g y)

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
