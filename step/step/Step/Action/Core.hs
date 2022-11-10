module Step.Action.Core where

import Step.Interface
import Step.LeftRight

-- The basics
import Data.Maybe (Maybe (..))
import Data.Function ((&))
import Data.Functor (Functor (..), (<$>), (<&>))
import Data.Function (($), (.), id)
import Data.Either (Either (..), either)
import Control.Monad (Monad (..))
import qualified Control.Monad as Monad
import Control.Applicative (Applicative (..))
import Data.Kind (Type)
import Prelude (error)
import Data.Functor.Const (Const)
import Data.Void (Void)

-- Optics
import Optics (Iso', over, iso)
import qualified Optics

-- Transformers
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Reader (ReaderT (..))

-- Streaming
import SupplyChain (Job, (>-), Vendor)
import qualified SupplyChain.Alter as Alter
import qualified SupplyChain.Vendor as Vendor

-- Etc
import GHC.TypeLits (TypeError, ErrorMessage (Text))


type Action =
     Type -- ^ Chunk of input
  -> (Type -> Type)
  -> Type -- ^ Param
  -> Type -- ^ Result
  -> Type


-- Simple actions that are just a newtype for ResettingSequence:

type Any :: Action
type Query :: Action
type Sure :: Action
type SureQuery :: Action

-- | The most general of the actions
newtype Any c m r a = Any (r -> ResettingSequence (CommittableChunkStream c) m (Either r a))
    deriving (Functor, Applicative, Monad)
        via ExceptT r (ReaderT r (ResettingSequence (CommittableChunkStream c) m))

-- | Like 'Any', but cannot move the cursor
newtype Query c m r a = Query (r -> ResettingSequence (ResettableTerminableStream c) m (Either r a))
    deriving (Functor, Applicative, Monad)
        via ExceptT r (ReaderT r (ResettingSequence (ResettableTerminableStream c) m))

-- | Always succeeds
newtype Sure c m r a = Sure (r -> ResettingSequence (CommittableChunkStream c) m a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT r (ResettingSequence (CommittableChunkStream c) m))

-- | Always succeeds, does not move the cursor
newtype SureQuery c m r a = SureQuery (r -> ResettingSequence (ResettableTerminableStream c) m a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT r (ResettingSequence (ResettableTerminableStream c) m))


type Atom :: Action

-- | Fails noncommittally; see 'try'
newtype Atom c m r a = Atom (Query c m r (Sure c m r a))
    deriving stock Functor

instance (TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom c m r) where
    pure = error "unreachable"
    (<*>) = error "unreachable"


class IsAction p => IsResettingSequence p up e | p -> up e
  where
    gen :: Iso' (p c m r a) (r -> ResettingSequence (up c) m (e r a))

instance IsResettingSequence Any CommittableChunkStream Either where gen = Optics.coerced
instance IsResettingSequence Sure CommittableChunkStream Right where gen = Optics.coerced
instance IsResettingSequence Query ResettableTerminableStream Either where gen = Optics.coerced
instance IsResettingSequence SureQuery ResettableTerminableStream Right where gen = Optics.coerced

run :: IsResettingSequence p up e => r -> p c m r a -> Job (up c) m (e r a)
run r = resettingSequenceJob . ($ r) . Optics.view gen

act :: IsResettingSequence p up e => (r -> Job (up c) m (e r a)) -> p c m r a
act f = Optics.review gen \x -> ResettingSequenceJob $ f x


rsj :: Optics.Iso
  (ResettingSequence up1 action1 a1)
  (ResettingSequence up2 action2 a2)
  (Job up1 action1 a1)
  (Job up2 action2 a2)
rsj = iso resettingSequenceJob ResettingSequenceJob


class CanSucceed (act :: Action) where
    pure' :: a -> act c m r a

instance CanSucceed Any where pure' = pure
instance CanSucceed Sure where pure' = pure
instance CanSucceed Query where pure' = pure
instance CanSucceed SureQuery where pure' = pure
instance CanSucceed Atom where pure' = Atom . pure' . pure'


class IsAction (act :: Action) where
    actionMap :: (forall x. m x -> m' x) -> act c m r a -> act c m' r a
    paramMap :: (r -> r) -> act c m r a -> act c m r a

instance IsAction Any where
    actionMap f (Any x) = Any $ x & (.) (over rsj (Alter.job' (Alter.perform' f)))
    paramMap f (Any x) = Any $ x . f

instance IsAction Sure where
    actionMap f (Sure x) = Sure $ x & (.) (over rsj (Alter.job' (Alter.perform' f)))
    paramMap f (Sure x) = Sure $ x . f

instance IsAction Query where
    actionMap f (Query x) = Query $ x & (.) (over rsj (Alter.job' (Alter.perform' f)))
    paramMap f (Query x) = Query (x . f)

instance IsAction SureQuery where
    actionMap f (SureQuery x) = SureQuery $ x & (.) (over rsj (Alter.job' (Alter.perform' f)))
    paramMap f (SureQuery x) = SureQuery $ x . f

instance IsAction Atom where
    actionMap f (Atom x) = Atom $ fmap (actionMap f) (actionMap f x)
    paramMap f (Atom x) = Atom $ fmap (paramMap f) (paramMap f x)


-- | Action that can be tried noncommittally

class (IsAction act, IsAction try) =>
    Atomic (act :: Action) (try :: Action)
    | act -> try
  where
    try :: act c m r a -> try c m r (Maybe a)

instance Atomic Atom Sure where
    try (Atom q) = act \r -> (Vendor.map stepCast >- run r q) >>= \case
        Left _ -> pure $ right Nothing
        Right (Sure x) -> resettingSequenceJob $ fmap (right . Just) (x r)

instance Atomic Query SureQuery where
    try q = act \x -> (run x q <&> right . either (\_ -> Nothing) Just)

instance Atomic Sure Sure where
    try x = Just <$> x


-- | Action subtype relationship

class (IsAction act1, IsAction act2) =>
    Is (act1 :: Action) (act2 :: Action)
  where
    cast :: act1 c m r a -> act2 c m r a

-- | Same as 'cast', but with type parameters reordered so that the action we're casting to is first, which is more convenient for type application in some circumstances
castTo :: forall act2 act1 c m r a. Is act1 act2 => act1 c m r a -> act2 c m r a
castTo = cast @act1 @act2

cast2 :: forall act2 act1 f c m r a. Is act1 act2 => Functor f => f (act1 c m r a) -> f (act2 c m r a)
cast2 = fmap (castTo @act2)

-- Everything is itself

instance {-# overlappable #-} IsAction a => Is a a where
    cast = id

-- Casting actions via casting steps

instance Is SureQuery Sure where
    cast x = act \r -> Vendor.map stepCast >- run r x

instance Is Query Any where
    cast x = act \r -> Vendor.map stepCast >- run r x

instance Is SureQuery Query where
    cast x = act \r -> run r x <&> toEither

instance Is Sure Any where
    cast x = act \r -> run r x <&> toEither

instance Is SureQuery Any where
    cast x = act \r -> Vendor.map stepCast >- run r x <&> toEither

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


{-| The type @a >> b@ is type of the expression @a >> b@.

    This function is not commutative (@a >> b@ is not the same as @b >> a@)
    because whether an atomic action's atomicity is preserved depends on
    the order of the composition in some cases.
-}

type family (act1 :: Action) >> (act2 :: Action) :: Action
  where

    -- Joining with SureQuery has no effect on the type
    SureQuery >> k = k
    k >> SureQuery = k

    -- Properties other than atomicity are closed under composition.
    Query >> Query = Query
    Sure >> Sure = Sure

    -- When an atomic step is followed by an infallible step, atomicity is preserved.
    Atom >> Sure = Atom
    -- (>> SureQuery) has already been covered above.

    -- When an atomic step is preceded by a query, atomicity is preserved.
    Query >> Atom = Atom
    Query >> Sure = Atom
    -- (SureQuery >>) has already been covered above.

    -- All other combinations degrade to Any.
    _ >> _ = Any


class Join (act1 :: Action) (act2 :: Action) where
    join :: act1 c m r (act2 c m r a) -> (act1 >> act2) c m r a

-- | See also: '(Step.Do.>>=)'
bindAction :: (Join act1 act2, Functor (act1 c m r), act1 >> act2 ~ act3) =>
    act1 c m r a -> (a -> act2 c m r b) -> act3 c m r b
bindAction x f = join (fmap f x)
infixl 1 `bindAction`

-- Any >> ...

instance Join Any Any where
    join = Monad.join

instance Join Any Atom where
    join = join @Any @Any . cast2 @Any

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

instance Join Atom Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any

instance Join Atom SureQuery where
    join = Atom . fmap (join @Sure @SureQuery) . (\(Atom q) -> q)

instance Join Atom Sure where
    join = Atom . fmap (join @Sure @Sure) . (\(Atom q) -> q)

-- Query >> ...

instance Join Query Any where
    join = join @Any @Any . castTo @Any

instance Join Query Atom where
    join = Atom . join . fmap (\(Atom q) -> q)

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
| 'Query'      |          | No         | No         |
+--------------+----------+------------+------------+
| 'Atom'       |          |            | No         |
+--------------+----------+------------+------------+
| 'Sure'       | Yes      |            |            |
+--------------+----------+------------+------------+
| 'SureQuery'  | Yes      | No         | No         |
+--------------+----------+------------+------------+
| 'Any'        |          |            |            |
+--------------+----------+------------+------------+

-}
