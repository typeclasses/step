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
    AgentAction :: m r -> Agent am bm m r

    -- | Request of type @a x@ + continuation from response type @x@
    AgentRequest :: a r -> Agent ('Just a) bm m r

    -- | Continuation from request type @b x@ producing a response of type @x@
    AgentServe :: (forall x. b x -> Agent am 'Nothing m (r, x)) -> Agent am ('Just b) m r

type Daemon am b m = Agent am ('Just b) m Void

instance Functor m => Functor (Agent am bm m) where
    fmap f = \case
        AgentAction x -> AgentAction (fmap f x)
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
    AgentAction x -> x
    AgentBind x f -> runAgent x >>= (runAgent . f)

type Connection am b cm m r = Agent am cm m (Daemon am b m, r)

connect :: Functor m => Daemon am b m -> Agent ('Just b) cm m r -> Connection am b cm m r

connect up (AgentPure x) = fmap (up,) (AgentPure x)
connect up (AgentAction x) = AgentAction (x <&> (up,))
connect up (AgentBind y g) = connect up y >>= \(up', z) -> connect up' (g z)
connect up (AgentServe f) = AgentServe \x -> connect up (f x) <&> \(up', (r, y)) -> ((up', r), y)

connect (AgentBind (AgentServe f) g) (AgentRequest x) = relaxAgentDown (f x) <&> \(r, y) -> (g r, y)

-- These cases involving 'absurd' don't really happen, but they satisfy the type checker
connect (AgentAction x) (AgentRequest _) = AgentAction (fmap absurd x)
connect (AgentServe f) (AgentRequest x) = relaxAgentDown (f x) >>= \(r, _) -> absurd r

(>->) :: Functor m => Daemon am b m -> Agent ('Just b) cm m r -> Agent am cm m r
a >-> b = fmap snd (connect a b)

relaxAgentDown :: Functor m => Agent am 'Nothing m r -> Agent am cm m r
relaxAgentDown = r
  where
    r :: Functor m => Agent am 'Nothing m r -> Agent am cm m r
    r = \case
      AgentRequest x -> AgentRequest x
      AgentAction x -> AgentAction x
      AgentBind x f -> AgentBind (r x) (fmap r f)
      AgentPure x -> AgentPure x

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
