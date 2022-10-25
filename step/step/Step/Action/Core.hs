module Step.Action.Core where

import Step.Interface

-- The basics
import Data.Maybe (Maybe (..))
import Data.Functor (Functor (..), (<$>), (<&>))
import Data.Function (($), (.), id)
import Data.Either (Either (..), either)
import Control.Monad (Monad (..))
import qualified Control.Monad as Monad
import Control.Applicative (Applicative (..))
import Data.Kind (Type)
import Prelude (error)

-- Optics
import Optics (Iso')
import qualified Optics

-- Transformers
import Control.Monad.Trans.Except (ExceptT (..))

-- Streaming
import SupplyChain (Factory, (>->), NoInterface, noVendor)
import qualified SupplyChain

-- Etc
import GHC.TypeLits (TypeError, ErrorMessage (Text))


type Action =
     Type -- ^ Chunk of input
  -> SupplyChain.Action
  -> Type -- ^ Error
  -> Type -- ^ Result
  -> Type


-- Simple actions that are just a newtype for ResettingSequence:

type Any :: Action
type Query :: Action
type Sure :: Action
type SureQuery :: Action

-- | The most general of the actions
newtype Any c m e a = Any (ResettingSequence (CommittableChunkStream c) m (Either e a))
    deriving (Functor, Applicative, Monad)
        via ExceptT e (ResettingSequence (CommittableChunkStream c) m)

-- | Like 'Any', but cannot move the cursor
newtype Query c m e a = Query (ResettingSequence (ResettableTerminableStream c) m (Either e a))
    deriving (Functor, Applicative, Monad)
        via ExceptT e (ResettingSequence (ResettableTerminableStream c) m)

-- | Always succeeds
newtype Sure c m e a = Sure (ResettingSequence (CommittableChunkStream c) m a)
    deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds, does not move the cursor
newtype SureQuery c m e a = SureQuery (ResettingSequence (ResettableTerminableStream c) m a)
    deriving newtype (Functor, Applicative, Monad)


-- Actions defined in terms of others:

type Atom :: Action
type Move :: Action
type AtomicMove :: Action

-- | Fails noncommittally; see 'try'
newtype Atom c m e a = Atom (Query c m e (Sure c m e a))
    deriving stock Functor

-- | Always moves the cursor
newtype Move c m e a = Move (Any c m e a)
    deriving stock Functor

-- | Always moves the cursor, is atomic
newtype AtomicMove c m e a = AtomicMove (Atom c m e a)
    deriving stock Functor

instance (TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom c m e) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

instance (TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
    Applicative (Move c m e)
  where
    pure = error "unreachable"
    (<*>) = error "unreachable"

instance (TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove c e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"


-- Failure action:

type Failure :: Action

newtype Failure c m e a = Failure (Factory NoInterface m e)
    deriving stock Functor

instance (TypeError ('Text "Failure cannot be Applicative because 'pure' would succeed")) => Applicative (Failure c m e) where
    pure = error "unreachable"
    (<*>) = error "unreachable"


class IsAction p =>
    IsResettingSequence p c m e a up product
    | p c m e a -> up product
  where
    walk :: Iso' (p c m e a) (ResettingSequence up m product)

instance IsResettingSequence Any c m e a (CommittableChunkStream c) (Either e a) where
    walk = Optics.coerced

instance IsResettingSequence Sure c m e a (CommittableChunkStream c) a where
    walk = Optics.coerced

instance IsResettingSequence Query c m e a (ResettableTerminableStream c) (Either e a) where
    walk = Optics.coerced

instance IsResettingSequence SureQuery c m e a (ResettableTerminableStream c) a where
    walk = Optics.coerced


run :: IsResettingSequence p c m e a up product => p c m e a -> Factory up m product
run = (\(ResettingSequence x) -> x) . Optics.view walk


act :: IsResettingSequence p c m e a up product => Factory up m product -> p c m e a
act = Optics.review walk . ResettingSequence


class IsAction (act :: Action) where
    actionMap :: (forall x. m x -> m' x) -> act c m e a -> act c m' e a

instance IsAction Any where
    actionMap f (Any (ResettingSequence x)) = Any (ResettingSequence (SupplyChain.actionMap f x))

instance IsAction Sure where
    actionMap f (Sure (ResettingSequence x)) = Sure (ResettingSequence (SupplyChain.actionMap f x))

instance IsAction Query where
    actionMap f (Query (ResettingSequence x)) = Query (ResettingSequence (SupplyChain.actionMap f x))

instance IsAction SureQuery where
    actionMap f (SureQuery (ResettingSequence x)) = SureQuery (ResettingSequence (SupplyChain.actionMap f x))

instance IsAction Atom where
    actionMap f (Atom x) = Atom (fmap (actionMap f) (actionMap f x))

instance IsAction Move where
    actionMap f (Move x) = Move (actionMap f x)

instance IsAction AtomicMove where
    actionMap f (AtomicMove x) = AtomicMove (actionMap f x)

instance IsAction Failure where
    actionMap f (Failure x) = Failure (SupplyChain.actionMap f x)


-- | Action that can be tried noncommittally

class (IsAction act, IsAction try) =>
    Atomic (act :: Action) (try :: Action)
    | act -> try
  where
    try :: act c m e a -> try c m e (Maybe a)

instance Atomic Atom Sure where
    try (Atom q) =
        act (SupplyChain.map stepCast >-> run q) >>= \case
            Left _ -> pure Nothing
            Right x -> fmap Just x

instance Atomic AtomicMove Sure where
    try (AtomicMove a) = try a

instance Atomic Query SureQuery where
    try q = act (run q <&> either (\_ -> Nothing) Just)


-- | Unsafe coercion to action that always moves

class Is act2 act1 =>
    AssumeMovement (act1 :: Action) (act2 :: Action)
    | act1 -> act2
  where
    assumeMovement :: act1 c es e a -> act2 c es e a

instance AssumeMovement Any Move where
    assumeMovement = Move

instance AssumeMovement Atom AtomicMove where
    assumeMovement = AtomicMove


-- | Action subtype relationship

class (IsAction act1, IsAction act2) =>
    Is (act1 :: Action) (act2 :: Action)
  where
    cast :: act1 c es e a -> act2 c es e a

-- | Same as 'cast', but with type parameters reordered so that the action we're casting to is first, which is more convenient for type application in some circumstances
castTo :: forall act2 act1 c es e a. Is act1 act2 => act1 c es e a -> act2 c es e a
castTo = cast @act1 @act2

cast2 :: forall act2 act1 f c es e a. Is act1 act2 => Functor f => f (act1 c es e a) -> f (act2 c es e a)
cast2 = fmap (castTo @act2)

-- Everything is itself

instance {-# overlappable #-} IsAction a => Is a a where
    cast = id

-- Casting actions via casting steps

instance Is SureQuery Sure where
    cast x = act (SupplyChain.map stepCast >-> run x)

instance Is Query Any where
    cast x = act (SupplyChain.map stepCast >-> run x)

instance Is SureQuery Query where
    cast x = act (run x <&> Right)

instance Is Sure Any where
    cast x = act (run x <&> Right)

instance Is SureQuery Any where
    cast x = act ((SupplyChain.map stepCast >-> run x) <&> Right)

-- Casting to Atom

instance Is SureQuery Atom where
    cast = Atom . fmap return . castTo @Query

instance Is Query Atom where
    cast = Atom . fmap return

instance Is Sure Atom where
    cast = Atom . return

-- Casting out of atomicity

instance Is Atom Any where
    cast (Atom x) = Monad.join (cast @Sure @Any <$> cast @Query @Any x)

instance Is AtomicMove Move where
    cast (AtomicMove x) = Move (cast @Atom @Any x)

-- Casting out of movement

instance Is Move Any where
    cast (Move x) = x

instance Is AtomicMove Atom where
    cast (AtomicMove x) = x

-- Casting out of both atomicity and movement

instance Is AtomicMove Any where
    cast = cast . castTo @Move

-- Casting out of failure

instance Is Failure Any where
    cast (Failure x) = act ((noVendor >-> x) <&> Left)

instance Is Failure Query where
    cast (Failure x) = act ((noVendor >-> x) <&> Left)

instance Is Failure Move where
    cast (Failure x) = Move $ act ((noVendor >-> x) <&> Left)

instance Is Failure Atom where
    cast (Failure x) = Atom $ act ((noVendor >-> x) <&> Left)

instance Is Failure AtomicMove where
    cast (Failure x) = AtomicMove $ Atom $ act ((noVendor >-> x) <&> Left)


{-| The type @a >> b@ is type of the expression @a >> b@.

    This function is not commutative (@a >> b@ is not the same as @b >> a@)
    because whether an atomic action's atomicity is preserved depends on
    the order of the composition in some cases.
-}

type family (act1 :: Action) >> (act2 :: Action) :: Action
  where

    -- When failure is first, the second step is irrelevant.
    Failure >> k = Failure

    -- When failure is second, sureness and atomicity are lost.
    Sure >> Failure = Any
    SureQuery >> Failure = Query
    Atom >> Failure = Any
    AtomicMove >> Failure = Move
    k >> Failure = k

    -- Joining with SureQuery has no effect on the type
    SureQuery >> k = k
    k >> SureQuery = k

    -- Properties other than atomicity are closed under composition.
    Query >> Query = Query
    Sure >> Sure = Sure
    -- (Move >> Move = Move) is covered later below.

    -- When an atomic step is followed by an infallible step, atomicity is preserved.
    Atom >> Sure = Atom
    AtomicMove >> Sure = AtomicMove
    -- (>> SureQuery) has already been covered above.

    -- When an atomic step is preceded by a query, atomicity is preserved.
    Query >> Atom = Atom
    Query >> Sure = Atom
    Query >> AtomicMove = AtomicMove
    -- (SureQuery >>) has already been covered above.

    -- Movement of a part implies movement of the whole.
    k >> Move = Move
    Move >> k = Move

    -- When AtomicMove loses atomicity, it degrades to Move.
    AtomicMove >> _ = Move
    _ >> AtomicMove = Move

    -- All other combinations degrade to Any.
    _ >> _ = Any


class Join (act1 :: Action) (act2 :: Action) where
    join :: act1 c es e (act2 c es e a) -> (act1 >> act2) c es e a

-- | See also: '(Step.Do.>>=)'
bindAction :: (Join act1 act2, Functor (act1 c es e), act1 >> act2 ~ act3) =>
    act1 c es e a -> (a -> act2 c es e b) -> act3 c es e b
bindAction x f = join (fmap f x)
infixl 1 `bindAction`

-- Any >> ...

instance Join Any Any where
    join = Monad.join

instance Join Any Atom where
    join = join @Any @Any . cast2 @Any

instance Join Any AtomicMove where
    join = assumeMovement . join @Any @Any . cast2 @Any

instance Join Any Failure where
    join = join @Any @Any . cast2 @Any

instance Join Any Move where
    join = assumeMovement . join @Any @Any . cast2 @Any

instance Join Any Query where
    join = join @Any @Any . cast2 @Any

instance Join Any SureQuery where
    join = join @Any @Any . cast2 @Any

instance Join Any Sure where
    join = join @Any @Any . cast2 @Any

-- Atom >> ...

instance Join Atom Any where
    join = join @Any @Any . castTo @Any

instance Join Atom Atom where
    join = join @Any @Any . castTo @Any . cast2 @Any

instance Join Atom AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any

instance Join Atom Failure where
    join = join @Any @Any . castTo @Any . cast2 @Any

instance Join Atom Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any

instance Join Atom Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any

instance Join Atom SureQuery where
    join = Atom . fmap (join @Sure @SureQuery) . (\(Atom q) -> q)

instance Join Atom Sure where
    join = Atom . fmap (join @Sure @Sure) . (\(Atom q) -> q)

-- AtomicMove >> ...

instance Join AtomicMove Any where
    join = assumeMovement . join . castTo @Atom

instance Join AtomicMove Atom where
    join = assumeMovement . join . castTo @Atom

instance Join AtomicMove AtomicMove where
    join = join . castTo @Atom

instance Join AtomicMove Failure where
    join = assumeMovement . join . castTo @Atom

instance Join AtomicMove Move where
    join = join . castTo @Atom

instance Join AtomicMove Query where
    join = assumeMovement . join . castTo @Atom

instance Join AtomicMove SureQuery where
    join = assumeMovement . join . castTo @Atom

instance Join AtomicMove Sure where
    join = assumeMovement . join . castTo @Atom

-- Failure >> ...

instance Join Failure Any where
    join (Failure f) = Failure f

instance Join Failure Atom where
    join (Failure f) = Failure f

instance Join Failure AtomicMove where
    join (Failure f) = Failure f

instance Join Failure Failure where
    join (Failure f) = Failure f

instance Join Failure Move where
    join (Failure f) = Failure f

instance Join Failure Query where
    join (Failure f) = Failure f

instance Join Failure Sure where
    join (Failure f) = Failure f

instance Join Failure SureQuery where
    join (Failure f) = Failure f

-- Move >> ...

instance Join Move Any where
    join = assumeMovement . join . castTo @Any

instance Join Move Atom where
    join = assumeMovement . join . castTo @Any

instance Join Move AtomicMove where
    join = join . castTo @Any

instance Join Move Failure where
    join = assumeMovement . join . castTo @Any

instance Join Move Move where
    join = join . castTo @Any

instance Join Move Query where
    join = assumeMovement . join . castTo @Any

instance Join Move SureQuery where
    join = assumeMovement . join . castTo @Any

instance Join Move Sure where
    join = assumeMovement . join . castTo @Any

-- Query >> ...

instance Join Query Any where
    join = join @Any @Any . castTo @Any

instance Join Query Atom where
    join = Atom . join . fmap (\(Atom q) -> q)

instance Join Query AtomicMove where
    join = assumeMovement . join @Query @Atom . cast2 @Atom

instance Join Query Failure where
    join = join @Query @Query . cast2 @Query

instance Join Query Move where
    join = assumeMovement . join @Query @Any . cast2 @Any

instance Join Query Query where
    join = Monad.join

instance Join Query SureQuery where
    join = join @Query @Query . cast2 @Query

instance Join Query Sure where
    join = Atom

-- Sure >> ...

instance Join Sure Any where
    join = join @Any @Any . castTo @Any

instance Join Sure Atom where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any

instance Join Sure AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any

instance Join Sure Failure where
    join = join @Any @Any . castTo @Any . cast2 @Any

instance Join Sure Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any

instance Join Sure Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any

instance Join Sure SureQuery where
    join = join @Sure @Sure . cast2 @Sure

instance Join Sure Sure where
    join = Monad.join

 -- SureQuery >> ...

instance Join SureQuery Any where
    join = join @Any @Any . castTo @Any

instance Join SureQuery Atom where
    join = Atom . join @SureQuery @Query . fmap (\(Atom q) -> q)

instance Join SureQuery AtomicMove where
    join = assumeMovement . join . cast2 @Atom

instance Join SureQuery Failure where
    join = join @Query @Failure . castTo @Query

instance Join SureQuery Move where
    join = assumeMovement . join. cast2 @Any

instance Join SureQuery Query where
    join = join @Query @Query . castTo @Query

instance Join SureQuery Sure where
    join = join @Sure @Sure . castTo @Sure

instance Join SureQuery SureQuery where
    join = Monad.join


{- $subtyping

Arrows in the graph below indicate permitted use of 'cast'. (Not pictured: 'Fail')

![Action subtyping graph](graphics/action-subtyping.svg)

-}


{- $types

+--------------+----------+------------+------------+
|              | Succeeds | Advances   | Advances   |
|              |          | on success | on failure |
+--------------+----------+------------+------------+
| 'Move'       |          | Yes        |            |
+--------------+----------+------------+------------+
| 'Query'      |          | No         | No         |
+--------------+----------+------------+------------+
| 'Atom'       |          |            | No         |
+--------------+----------+------------+------------+
| 'AtomicMove' |          | Yes        | No         |
+--------------+----------+------------+------------+
| 'Sure'       | Yes      |            |            |
+--------------+----------+------------+------------+
| 'SureQuery'  | Yes      | No         | No         |
+--------------+----------+------------+------------+
| 'Fail'       | No       | No         | No         |
+--------------+----------+------------+------------+
| 'Any'        |          |            |            |
+--------------+----------+------------+------------+

-}
