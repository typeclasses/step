module Step.Action.Core where

import Step.Interface.Core

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

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

-- Transformers
import qualified Control.Monad.Reader as MTL
import Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as MTL

-- Streaming
import SupplyChain (Factory, (>->))
import qualified SupplyChain

-- Etc
import GHC.TypeLits (TypeError, ErrorMessage (Text))


type Action =
     Type -- ^ Chunk of input
  -> SupplyChain.Action
  -> Type -- ^ Error
  -> Type -- ^ Result
  -> Type


-- Simple actions that are just a newtype for Walk:

type Any :: Action
type Query :: Action
type Sure :: Action
type SureQuery :: Action

-- | The most general of the actions
newtype Any c m e a = Any (ExceptT e (Walk 'RW c m) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Like 'Any', but cannot move the cursor
newtype Query c m e a = Query (ExceptT e (Walk 'R c m) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds
newtype Sure c m e a = Sure (Walk 'RW c m a)
    deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds, does not move the cursor
newtype SureQuery c m e a = SureQuery (Walk 'R c m a)
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

newtype Failure c m e a = Failure (m e)
    deriving stock Functor

instance (TypeError ('Text "Failure cannot be Applicative because 'pure' would succeed")) => Applicative (Failure c m e) where
    pure = error "unreachable"
    (<*>) = error "unreachable"


-- | Action that can return a value and do nothing else

class Trivial (act :: Action) where
    trivial :: a -> act c e m a

instance Trivial Any where
    trivial x = Any (return x)

instance Trivial Query where
    trivial x = Query (return x)

instance Trivial Sure where
    trivial x = Sure (return x)

instance Trivial SureQuery where
    trivial x = SureQuery (return x)

instance Trivial Atom where
    trivial x = Atom (Query (return (trivial x)))


-- | Action that can fail

class Fallible (act :: Action) where
    failAction :: e -> act c m e a
    failActionM :: m e -> act c m e a

instance Fallible Any where
    failAction e = Any (MTL.throwE e)
    failActionM m = Any (MTL.lift (Walk $ SupplyChain.perform m) >>= MTL.throwE)

instance Fallible Query where
    failAction e = Query (MTL.throwE e)
    failActionM m = Query (MTL.lift (Walk $ SupplyChain.perform m) >>= MTL.throwE)

instance Fallible Move where
    failAction e = Move $ Any (MTL.throwE e)
    failActionM m = Move $ Any (MTL.lift (Walk $ SupplyChain.perform m) >>= MTL.throwE)

instance Fallible Atom where
    failAction e = Atom $ Query (MTL.throwE e)
    failActionM m = Atom $ Query (MTL.lift (Walk $ SupplyChain.perform m) >>= MTL.throwE)

instance Fallible AtomicMove where
    failAction e = AtomicMove $ Atom $ Query (MTL.throwE e)
    failActionM m = AtomicMove $ Atom $ Query (MTL.lift (Walk $ SupplyChain.perform m) >>= MTL.throwE)


-- | Action that can be tried noncommittally

class Atomic (act :: Action) (try :: Action) | act -> try where
    try :: act c m e a -> try c m e (Maybe a)

instance Atomic Atom Sure where
    try (Atom (Query q)) =
        Sure (Walk (SupplyChain.map stepCast >-> (let Walk q' = MTL.runExceptT q in q'))) >>= \case
            Left _ -> pure Nothing
            Right x -> fmap Just x

instance Atomic AtomicMove Sure where
    try (AtomicMove a) = try a

instance Atomic Query SureQuery where
    try (Query q) = SureQuery (MTL.runExceptT q <&> either (\_ -> Nothing) Just)


-- | Unsafe coercion to action that always moves

class Is act2 act1 => AssumeMovement (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeMovement :: act1 c es e a -> act2 c es e a

instance AssumeMovement Any Move where
    assumeMovement = Move

instance AssumeMovement Atom AtomicMove where
    assumeMovement = AtomicMove


-- | Action subtype relationship

class Is (act1 :: Action) (act2 :: Action) where
    cast :: act1 c es e a -> act2 c es e a

-- | Same as 'cast', but with type parameters reordered so that the action we're casting to is first, which is more convenient for type application in some circumstances
castTo :: forall act2 act1 c es e a. Is act1 act2 => act1 c es e a -> act2 c es e a
castTo = cast @act1 @act2

cast2 :: forall act2 act1 f c es e a. Is act1 act2 => Functor f => f (act1 c es e a) -> f (act2 c es e a)
cast2 = fmap (castTo @act2)

-- Everything is itself

instance {-# overlappable #-} Is a a where
    cast = id

-- Casting actions via casting steps

instance Is SureQuery Sure where
    cast (SureQuery (Walk x)) = Sure (Walk (SupplyChain.map stepCast >-> x))

instance Is Query Any where
    cast (Query (ExceptT (Walk x))) = Any (ExceptT (Walk (SupplyChain.map stepCast >-> x)))

instance Is SureQuery Query where
    cast (SureQuery x) = Query (MTL.lift x)

instance Is Sure Any where
    cast (Sure x) = Any (MTL.lift x)

instance Is SureQuery Any where
    cast (SureQuery (Walk x)) = Any (MTL.lift (Walk (SupplyChain.map stepCast >-> x)))

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

instance Fallible a => Is Failure a where
    cast (Failure x) = failActionM x


{-| The type @a >> b@ is type of the expression @a >> b@.

    This function is mostly not commutative
    (@a >> b@ is not the same as @b >> a@)
    because whether an atomic action's atomicity is preserved
    depends on the order of the composition in some cases.
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

infixl 1 `bindAction`
bindAction :: (Join act1 act2, Functor (act1 c es e), act1 >> act2 ~ act3) =>
    act1 c es e a -> (a -> act2 c es e b) -> act3 c es e b
bindAction x f = join (fmap f x)

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
