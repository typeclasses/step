module Agency where

import Relude hiding (Proxy)

data Agent (am :: Maybe (Type -> Type)) (bm :: Maybe (Type -> Type)) m r where

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

data Client x a bm m r = Client
    { clientRequest :: a x
        -- ^ A client makes a request of type @a x@
    , clientContinuation :: x -> Agent ('Just a) bm m r
        -- ^ A client receives a response of type @x@ from upstream
    }

newtype Server am b m r = Server
    { serverHandler :: forall x. b x -> Agent am 'Nothing m ( x, Agent am ('Just b) m r )
        -- ^ A server receives a @b x@ request from downstream and responds with @x@
    }

data Connect am b cm m r = Connect
    { connectServer :: Agent am ('Just b) m Void
    , connectClient :: Agent ('Just b) cm m r
    }

deriving stock instance Functor m => Functor (Client x a bm m)
deriving stock instance Functor m => Functor (Server am b m)
deriving stock instance Functor m => Functor (Agent am bm m)

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

generalizeAgentDownstream :: forall bm am m r. Functor m => Agent am 'Nothing m r -> Agent am bm m r
generalizeAgentDownstream = \case
    AgentPure x -> AgentPure x
    AgentBind x f -> AgentBind (generalizeAgentDownstream x) (fmap generalizeAgentDownstream f)
    AgentAction x -> AgentAction (fmap generalizeAgentDownstream x)
    AgentRequest x -> AgentRequest (generalizeClientDownstream x)

generalizeClientDownstream :: forall bm x a m r. Functor m => Client x a 'Nothing m r -> Client x a bm m r
generalizeClientDownstream (Client x f) = Client x (fmap generalizeAgentDownstream f)

generalizeConnectDownstream :: Functor m => Connect am b 'Nothing m r -> Connect am b cm m r
generalizeConnectDownstream (Connect a b) = Connect a (generalizeAgentDownstream b)

generalizeAgentUpstream :: Functor m => Agent 'Nothing bm m r -> Agent am bm m r
generalizeAgentUpstream = \case
    AgentPure x -> AgentPure x
    AgentBind x f -> AgentBind (generalizeAgentUpstream x) (fmap generalizeAgentUpstream f)
    AgentAction x -> AgentAction (fmap generalizeAgentUpstream x)
    AgentServe (Server f) -> AgentServe $ Server (fmap generalizeAgentUpstream . (fmap . fmap . fmap) generalizeAgentUpstream $ f)

generalizeConnectUpstream :: Functor m => Connect 'Nothing b cm m r -> Connect am b cm m r
generalizeConnectUpstream (Connect a b) = Connect (generalizeAgentUpstream a) b

(>->) :: forall am b cm m r. Functor m =>
    Agent am ('Just b) m Void -> Agent ('Just b) cm m r -> Agent am cm m r

_ >-> AgentPure x = AgentPure x

up >-> AgentAction x = AgentAction (fmap (up >->) x)

-- up >-> AgentServe (Server f) = AgentServe $ Server (\x -> up >-> (fmap . fmap) (up >->) (f x))

-- AgentAction ma >-> AgentRequest (Client x g) = _

-- AgentServe (Server f) >-> AgentRequest (Client x g) = _

-- AgentRequest (Client y h) >-> AgentRequest (Client x g) = AgentRequest $ Client y \z -> h z >-> down

_ >-> _ = _

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
