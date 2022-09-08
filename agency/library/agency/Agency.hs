module Agency where

import Relude hiding (Proxy)

data Agent (am :: Maybe (Type -> Type)) (bm :: Maybe (Type -> Type)) m r
  where

    Request :: a x -> (x -> Agent ('Just a) bm m r) -> Agent ('Just a) bm m r
        -- ^ When this agent makes a request of type @a x@, it receives from its upstream a response of type @x@.

    Serve :: (forall x. b x -> Agent am 'Nothing m (x, Agent am ('Just b) m r)) -> Agent am ('Just b) m r
        -- ^ When this agent receives a request of type @b x@, it response to its downstream with a response of type @x@.

    AgentM :: m (Agent am bm m r) -> Agent am bm m r
        -- ^ An action lifted from the base monad @m@

    AgentBind :: Agent am bm m x -> (x -> Agent am bm m r) -> Agent am bm m r
        -- ^ ('>>=') = AgentBind

    AgentPure :: r -> Agent am bm m r
        -- ^ 'pure' = AgentPure

deriving stock instance Functor m => Functor (Agent am bm m)

instance Functor m => Applicative (Agent am bm m) where
    pure = AgentPure
    a1 <*> a2 = AgentBind a1 \f -> a2 <&> \x -> f x
    a1 *> a2 = AgentBind a1 \_ -> a2

instance Functor m => Monad (Agent am bm m) where
    return = AgentPure
    (>>=) = AgentBind

type Server b m r = Agent 'Nothing ('Just b) m r

type Client a m r = Agent ('Just a) 'Nothing m r

type Proxy a b m r = Agent ('Just a) ('Just b) m r

generalizeDownstream :: forall bm am m r. Functor m => Agent am 'Nothing m r -> Agent am bm m r
generalizeDownstream = \case
    AgentPure x -> AgentPure x
    AgentBind x f -> AgentBind (generalizeDownstream x) (fmap generalizeDownstream f)
    AgentM x -> AgentM (fmap generalizeDownstream x)
    Request x f -> Request x (fmap generalizeDownstream f)

generalizeUpstream :: Functor m => Agent 'Nothing bm m r -> Agent am bm m r
generalizeUpstream = \case
    AgentPure x -> AgentPure x
    AgentBind x f -> AgentBind (generalizeUpstream x) (fmap generalizeUpstream f)
    AgentM x -> AgentM (fmap generalizeUpstream x)
    Serve f -> Serve (fmap generalizeUpstream . (fmap . fmap . fmap) generalizeUpstream $ f)

(>->) :: forall am b cm m r. Functor m => Agent am ('Just b) m r -> Agent ('Just b) cm m r -> Agent am cm m r
up >-> down =
  case down of
    AgentPure b -> AgentPure b
    AgentM b -> AgentM (fmap (up >->) b)
    Serve f -> Serve \(x :: c x) -> ((z up (f x) <&> \(y, c) -> (y, up >-> c)) :: Agent am 'Nothing m (x, Agent am cm m r))
    AgentBind x f -> _
    Request x g ->
      case up of
        AgentPure a -> AgentPure a
        AgentM ma -> _
        Serve f -> generalizeDownstream @cm (f x) >>= \(y, c) -> (c >-> g y)
        Request y h -> Request y \z -> h z >-> down
        AgentBind x f -> _

z :: Agent am ('Just b) m r
  -> Agent ('Just b) 'Nothing m (x, Agent ('Just b) ('Just b1) m r)
  -> Agent am 'Nothing m (x, Agent ('Just b) ('Just b1) m r)
z = _

{-

Thanks to:

pipes Proxy type for initial idea
https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76

streaming-benchmark for Bind constructor
https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12

-}
