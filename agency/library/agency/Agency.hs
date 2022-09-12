module Agency where

import Relude hiding (Proxy)

type Response = Type -> Type

type Context = Type -> Type

data Agent (am :: Maybe Response) (bm :: Maybe Response) (m :: Context) r where

    -- | Request of type @a x@ + continuation from response type @x@
    AgentRequest :: a r -> Agent ('Just a) bm m r

    -- | Continuation from request type @b x@ producing a response of type @x@
    AgentServe :: Server am b m r -> Agent am ('Just b) m r

    -- | Action lifted from the base monad @m@
    AgentAction :: m r -> Agent am bm m r

    -- | ('>>=')
    AgentBind :: Agent am bm m x -> (x -> Agent am bm m r) -> Agent am bm m r

    -- | ('<&>')
    AgentMap :: Agent am bm m x -> (x -> r) -> Agent am bm m r

    -- | 'pure'
    AgentPure :: r -> Agent am bm m r

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

instance Functor (Agent am bm m) where
    fmap f = \case
        AgentMap a g -> AgentMap a (f . g)
        a -> AgentMap a f

instance Applicative (Agent am bm m) where
    pure = AgentPure
    a1 <*> a2 = AgentBind a1 \f -> a2 <&> \x -> f x
    a1 *> a2 = AgentBind a1 \_ -> a2

instance Monad (Agent am bm m) where
    (>>=) = AgentBind

runAgent :: Monad m => Agent 'Nothing 'Nothing m r -> m r
runAgent = \case
    AgentPure x -> pure x
    AgentAction x -> x
    AgentMap x f -> runAgent x <&> f
    AgentBind x f -> runAgent x >>= (runAgent . f)

(>->) :: Daemon am b m -> Agent ('Just b) cm m r -> Agent am cm m r
_ >-> AgentPure x = AgentPure x
_ >-> AgentAction x = AgentAction x
up >-> AgentServe (Server f) = AgentServe $ Server \x -> up +>> f x
Daemon (AgentServe (Server f)) >-> AgentRequest x = reactionAgent' (f x) <&> yieldResponse

(+>>) ::
    Daemon am b m -> Reaction x ('Just b) c m r -> Reaction x am c m r
up +>> Reaction (AgentPure (Yield y down)) = Reaction do
    pure $ Yield y $ up >-> down
up +>> Reaction (AgentAction a) = Reaction do
    Yield y down <- AgentAction a
    pure $ Yield y $ up >-> down
Daemon (AgentServe (Server f)) +>> Reaction (AgentRequest x) = Reaction do
    Yield y up' <- reactionAgent $ f x
    reactionAgent $ Daemon up' +>> Reaction (pure y)

relaxAgentDown :: Agent am 'Nothing m r -> Agent am cm m r
relaxAgentDown = r
  where
    r :: Agent am 'Nothing m r -> Agent am cm m r
    r = \case
      AgentRequest x -> AgentRequest x
      AgentAction x -> AgentAction x
      AgentBind x f -> AgentBind (r x) (fmap r f)
      AgentMap x f -> AgentMap (r x) f
      AgentPure x -> AgentPure x

reactionAgent' :: Reaction x am b m r -> Agent am cm m (Yield x am b m r)
reactionAgent' = relaxAgentDown . reactionAgent

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
