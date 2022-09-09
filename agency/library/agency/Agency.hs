module Agency where

import Relude hiding (Proxy)

data Agent (am :: Maybe (Type -> Type)) (bm :: Maybe (Type -> Type)) m r
  where

    AgentRequest :: Client x a bm m r -> Agent ('Just a) bm m r
        -- ^ When this agent makes a request of type @a x@, it receives from its upstream a response of type @x@.

    AgentServe :: Server am b m r -> Agent am ('Just b) m r
        -- ^ When this agent receives a request of type @b x@, it response to its downstream with a response of type @x@.

    AgentAction :: m (Agent am bm m r) -> Agent am bm m r
        -- ^ An action lifted from the base monad @m@

    AgentBind :: Agent am bm m x -> (x -> Agent am bm m r) -> Agent am bm m r
        -- ^ ('>>=') = AgentBind

    AgentPure :: r -> Agent am bm m r
        -- ^ 'pure' = AgentPure

data Client x a bm m r =
  Client
    { clientRequest :: a x
    , clientContinuation :: x -> Agent ('Just a) bm m r
    }

newtype Server am b m r =
  Server
    ( forall x.
        b x
        -> Agent am 'Nothing m ( x, Agent am ('Just b) m r )
    )

deriving stock instance Functor m => Functor (Client x a bm m)
deriving stock instance Functor m => Functor (Server am b m)
deriving stock instance Functor m => Functor (Agent am bm m)

instance Functor m => Applicative (Agent am bm m) where
    pure = AgentPure
    a1 <*> a2 = AgentBind a1 \f -> a2 <&> \x -> f x
    a1 *> a2 = AgentBind a1 \_ -> a2

instance Functor m => Monad (Agent am bm m) where
    (>>=) = AgentBind

generalizeAgentDownstream :: forall bm am m r. Functor m => Agent am 'Nothing m r -> Agent am bm m r
generalizeAgentDownstream = \case
    AgentPure x -> AgentPure x
    AgentBind x f -> AgentBind (generalizeAgentDownstream x) (fmap generalizeAgentDownstream f)
    AgentAction x -> AgentAction (fmap generalizeAgentDownstream x)
    AgentRequest x -> AgentRequest (generalizeClientDownstream x)

generalizeClientDownstream :: forall bm x a m r. Client x a 'Nothing m r -> Client x a bm m r
generalizeClientDownstream (Client x f) = Client x f

generalizeAgentUpstream :: Functor m => Agent 'Nothing bm m r -> Agent am bm m r
generalizeAgentUpstream = \case
    AgentPure x -> AgentPure x
    AgentBind x f -> AgentBind (generalizeAgentUpstream x) (fmap generalizeAgentUpstream f)
    AgentAction x -> AgentAction (fmap generalizeAgentUpstream x)
    AgentServe (Server f) -> AgentServe $ Server (fmap generalizeAgentUpstream . (fmap . fmap . fmap) generalizeAgentUpstream $ f)

(>->) = agentToAgent

agentToAgent :: forall am b cm m r. Functor m => Agent am ('Just b) m Void -> Agent ('Just b) cm m r -> Agent am cm m r
agentToAgent up down =
    case down of
        AgentPure b -> AgentPure b
        AgentAction b -> AgentAction (fmap (up >->) b)
        AgentServe s -> AgentServe (agentToServer up s)
        AgentBind x f -> _
        AgentRequest x ->
            case up of
                AgentPure a -> absurd a
                AgentAction ma -> _
                AgentServe s -> serverToClient s x
                AgentRequest c -> clientToAgent c down
                AgentBind x f -> _

clientToAgent :: Client x a ('Just b) m Void -> Agent ('Just b) cm m r -> Agent ('Just a) cm m r
clientToAgent (Client y h) down = AgentRequest $ Client y \z -> h z >-> down

serverToClient :: forall x am b cm m r. Server am b m Void -> Client x b cm m r -> Agent am cm m r
serverToClient (Server f) (Client x g) = AgentRequest (generalizeClientDownstream @cm (f x)) >>= \(y, c) -> (c >-> g y)

agentToServer :: Functor m => Agent am ('Just b) m Void -> Server ('Just b) c m r -> Server am c m r
agentToServer up (Server f) =
    Server \x -> case f x of
        AgentPure (y, down') -> AgentPure (y, up >-> down')
        AgentAction b' -> AgentAction (((fmap . fmap . fmap) (up >->)) . fmap (up >->) $ b')

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
