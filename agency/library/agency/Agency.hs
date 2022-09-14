module Agency where

import Relude hiding (Proxy)

type Response = Type -> Type

type Context = Type -> Type

data Agent (am :: Maybe Response) (bm :: Maybe Response) (m :: Context) r where

    -- | 'pure'
    AgentPure :: r -> Agent am bm m r

    -- | ('>>=')
    AgentBind :: Agent am bm m x -> (x -> Agent am bm m r) -> Agent am bm m r

    -- | Action lifted from the base monad @m@
    AgentAction :: m (Agent am bm m r) -> Agent am bm m r

    -- | Request of type @a x@ + continuation from response type @x@
    AgentRequest :: a r -> Agent ('Just a) bm m r

    -- | Continuation from request type @b x@ producing a response of type @x@
    AgentServe :: Server am b m r -> Agent am ('Just b) m r

-- | A server receives a @b x@ request from downstream and responds with @x@
type Server (am :: Maybe Response) (b :: Response) (m :: Context) r =
    forall x. b x -> Reaction x am b m r

-- | A server in the process of reacting to a request
type Reaction x (am :: Maybe Response) (b :: Response) (m :: Context) r =
    Agent am 'Nothing m (x, Agent am ('Just b) m r)

type Daemon am b m = Agent am ('Just b) m Void

instance Functor m => Functor (Agent am bm m) where
    fmap f = \case
        AgentAction x -> AgentAction (fmap (fmap f) x)
        a -> AgentBind a (AgentPure . f)

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

(>->) :: Functor m => Daemon am b m -> Agent ('Just b) cm m r -> Agent am cm m r
_ >-> AgentPure x = AgentPure x
up >-> AgentAction x = AgentAction (x <&> (up >->))
up >-> AgentServe f = AgentServe \x -> up +>> f x
AgentServe f >-> AgentRequest x = relaxAgentDown (f x) <&> fst

(+>>) :: Functor m =>
    Daemon am b m -> Reaction x ('Just b) c m r -> Reaction x am c m r
up +>> AgentPure (y, down) = pure (y, up >-> down)
up +>> AgentAction a = AgentAction (a <&> (up +>>))
AgentServe f +>> AgentRequest x = f x >>= \(y, up') -> up' +>> pure y

relaxAgentDown :: Functor m => Agent am 'Nothing m r -> Agent am cm m r
relaxAgentDown = r
  where
    r :: Functor m => Agent am 'Nothing m r -> Agent am cm m r
    r = \case
      AgentRequest x -> AgentRequest x
      AgentAction x -> AgentAction (fmap r x)
      AgentBind x f -> AgentBind (r x) (fmap r f)
      AgentPure x -> AgentPure x

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
