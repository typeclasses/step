{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving, EmptyCase #-}

module Step.Action where

-- ⭕

import Step.Internal.Prelude

import Step.Nontrivial

import TypeLits (TypeError, ErrorMessage (Text))

import Step.RST

import Optics (coerced)
import Coerce (coerce)

import qualified Monad

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial

import Positive.Unsafe (Positive (PositiveUnsafe))
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed

import qualified NonEmpty

-- ⭕

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

-- ⭕

{- $subtypes

Arrows in the graph below indicate permitted use of 'cast'. (Not pictured: 'Fail')

![Action subtyping graph](graphics/action-subtyping.svg)

-}

-- ⭕

-- | The kind of all the action types
type Action =
       Type           -- ^ @xs@ - text
    -> Type           -- ^ @x@ - char
    -> Type           -- ^ @r@ - reader context
    -> Type           -- ^ @s@ - state context
    -> Type           -- ^ @e@ - error
    -> (Type -> Type) -- ^ @m@ - monadic context
    -> Type           -- ^ @a@ - produced upon success
    -> Type

-- ⭕ Classes

class
    ( forall xs x r s e m. Functor m => Functor (act xs x r s e m)
    , forall xs x r r' s e m a. Functor m => Contravariant (act xs x r s e m a) (act xs x r' s e m a) r r'
    ) =>
    BasicAction (act :: Action)

class Returnable (act :: Action) where
    trivial :: a -> act xs x r s e m a

class (BasicAction act, Returnable act, forall xs x r s e m. Functor m => Monad (act xs x r s e m)) =>
    MonadicAction (act :: Action)

class Fallible (act :: Action) where
    mapError :: Functor m => (e -> e') -> act xs x r s e m a -> act xs x r s e' m a

class Infallible (act :: Action) where
    mapError' :: Functor m => act xs x r s e m a -> act xs x r s e' m a

-- ⭕

data Mode = ReadWrite | ReadOnly

data family Commit (m :: Mode) :: Type -> Type

data instance Commit 'ReadOnly a
    deriving stock Functor

data instance Commit 'ReadWrite a = Commit (Positive Natural) a
    deriving stock Functor

-- ⭕

data Perfection = Perfect | Imperfect

type family Imperfection (p :: Perfection) (e :: Type) :: Type

type instance Imperfection 'Perfect e = Void

type instance Imperfection 'Imperfect e = e

-- ⭕

type Step :: Mode -> Perfection -> Action

data Step (mo :: Mode) (p :: Perfection) xs x r s e m a =
    Base_RST (RST r s m a)
  | Base_Reset a
  | Base_Next (Maybe (Nontrivial xs x) -> a)
  | Base_Fail (Imperfection p (Fail xs x r s e m a))
  | Base_Commit (Commit mo a)

-- class
--     ( forall xs x r s e m. Functor m => Functor (step xs x r s e m)
--     , forall xs x r r' s e m a. Contravariant (step xs x r s e m a) (step xs x r' s e m a) r r'
--     ) =>
--     IsStep (step :: Action)

-- instance IsStep (Step 'ReadOnly 'Perfect)
-- instance IsStep (Step 'ReadOnly 'Imperfect)
-- instance IsStep (Step 'ReadWrite 'Perfect)
-- instance IsStep (Step 'ReadWrite 'Imperfect)

instance Functor m => Functor (Step 'ReadOnly 'Perfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (fmap f x)
        Base_Fail x -> case x of {}
        Base_Commit x -> case x of {}

instance Functor m => Functor (Step 'ReadOnly 'Imperfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (fmap f x)
        Base_Fail x -> Base_Fail (fmap f x)
        Base_Commit x -> case x of {}

instance Functor m => Functor (Step 'ReadWrite 'Perfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (fmap f x)
        Base_Fail x -> case x of {}
        Base_Commit x -> Base_Commit (fmap f x)

instance Functor m => Functor (Step 'ReadWrite 'Imperfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (fmap f x)
        Base_Fail x -> Base_Fail (fmap f x)
        Base_Commit x -> Base_Commit (fmap f x)

instance Functor m => Contravariant (Step 'ReadOnly 'Perfect xs x r s e m a) (Step 'ReadOnly 'Perfect xs x r' s e m a) r r' where
    contramap f = \case
        Base_Fail x -> case x of {}
        Base_RST x -> Base_RST (contramap f x)
        Base_Commit x -> Base_Commit x
        Base_Reset x -> Base_Reset x
        Base_Next x -> Base_Next x

instance Functor m => Contravariant (Step 'ReadOnly 'Imperfect xs x r s e m a) (Step 'ReadOnly 'Imperfect xs x r' s e m a) r r' where
    contramap f = \case
        Base_Fail x -> Base_Fail (contramap f x)
        Base_RST x -> Base_RST (contramap f x)
        Base_Commit x -> case x of {}
        Base_Reset x -> Base_Reset x
        Base_Next x -> Base_Next x

instance Functor m => Contravariant (Step 'ReadWrite 'Perfect xs x r s e m a) (Step 'ReadWrite 'Perfect xs x r' s e m a) r r' where
    contramap f = \case
        Base_Fail x -> case x of {}
        Base_RST x -> Base_RST (contramap f x)
        Base_Reset x -> Base_Reset x
        Base_Next x -> Base_Next x
        Base_Commit x -> Base_Commit x

instance Functor m => Contravariant (Step 'ReadWrite 'Imperfect xs x r s e m a) (Step 'ReadWrite 'Imperfect xs x r' s e m a) r r' where
    contramap f = \case
        Base_Fail x -> Base_Fail (contramap f x)
        Base_RST x -> Base_RST (contramap f x)
        Base_Reset x -> Base_Reset x
        Base_Next x -> Base_Next x
        Base_Commit x -> Base_Commit x

-- ⭕

type Walk :: Mode -> Perfection -> Action

newtype Walk mo p xs x r s e m a = Walk{ unWalk :: Free (Step mo p xs x r s e m) a }

deriving newtype instance Functor m => Functor (Walk 'ReadWrite 'Perfect xs x r s e m)
deriving newtype instance Functor m => Functor (Walk 'ReadWrite 'Imperfect xs x r s e m)
deriving newtype instance Functor m => Functor (Walk 'ReadOnly 'Perfect xs x r s e m)
deriving newtype instance Functor m => Functor (Walk 'ReadOnly 'Imperfect xs x r s e m)

deriving newtype instance Functor m => Applicative (Walk 'ReadWrite 'Perfect xs x r s e m)
deriving newtype instance Functor m => Applicative (Walk 'ReadWrite 'Imperfect xs x r s e m)
deriving newtype instance Functor m => Applicative (Walk 'ReadOnly 'Perfect xs x r s e m)
deriving newtype instance Functor m => Applicative (Walk 'ReadOnly 'Imperfect xs x r s e m)

deriving newtype instance Functor m => Monad (Walk 'ReadWrite 'Perfect xs x r s e m)
deriving newtype instance Functor m => Monad (Walk 'ReadWrite 'Imperfect xs x r s e m)
deriving newtype instance Functor m => Monad (Walk 'ReadOnly 'Perfect xs x r s e m)
deriving newtype instance Functor m => Monad (Walk 'ReadOnly 'Imperfect xs x r s e m)

instance BasicAction (Walk 'ReadWrite 'Perfect)
instance BasicAction (Walk 'ReadWrite 'Imperfect)
instance BasicAction (Walk 'ReadOnly 'Perfect)
instance BasicAction (Walk 'ReadOnly 'Imperfect)

instance Functor m => Contravariant (Walk 'ReadWrite 'Perfect xs x r s e m a) (Walk 'ReadWrite 'Perfect xs x r' s e m a) r r' where
    contramap f (Walk a) = Walk $ hoistFree (contramap f) a

instance Functor m => Contravariant (Walk 'ReadWrite 'Imperfect xs x r s e m a) (Walk 'ReadWrite 'Imperfect xs x r' s e m a) r r' where
    contramap f (Walk a) = Walk $ hoistFree (contramap f) a

instance Functor m => Contravariant (Walk 'ReadOnly 'Perfect xs x r s e m a) (Walk 'ReadOnly 'Perfect xs x r' s e m a) r r' where
    contramap f (Walk a) = Walk $ hoistFree (contramap f) a

instance Functor m => Contravariant (Walk 'ReadOnly 'Imperfect xs x r s e m a) (Walk 'ReadOnly 'Imperfect xs x r' s e m a) r r' where
    contramap f (Walk a) = Walk $ hoistFree (contramap f) a

instance MonadicAction (Walk 'ReadWrite 'Perfect)
instance MonadicAction (Walk 'ReadWrite 'Imperfect)
instance MonadicAction (Walk 'ReadOnly 'Perfect)
instance MonadicAction (Walk 'ReadOnly 'Imperfect)

instance Returnable (Walk mo p) where
    trivial = Walk . Pure

-- ⭕

type Any :: Action

-- | The most general of the actions; a monadic combination of 'BaseRW'
newtype Any xs x r s e m a = Any{ unAny :: Walk 'ReadWrite 'Imperfect xs x r s e m a }
    deriving newtype (Functor, Applicative, Monad, Returnable, BasicAction, MonadicAction)

instance Functor m => Contravariant (Any xs x r s e m a) (Any xs x r' s e m a) r r' where
    contramap f = Any . contramap f . unAny

-- ⭕

type Query :: Action

-- | Like 'Any', but cannot move the cursor; a monadic combination of 'Step'
newtype Query xs x r s e m a = Query{ unQuery :: Walk 'ReadOnly 'Imperfect xs x r s e m a }
    deriving newtype (BasicAction, MonadicAction, Returnable, Functor, Applicative, Monad)

instance Functor m => Contravariant (Query xs x r s e m a) (Query xs x r' s e m a) r r' where
    contramap f = Query . contramap f . unQuery

-- ⭕

type Move :: Action

-- | Always moves the cursor
newtype Move xs x r s e m a = Move{ unMove :: Any xs x r s e m a }
    deriving newtype (Functor, BasicAction)

instance Functor m => Contravariant (Move xs x r s e m a) (Move xs x r' s e m a) r r' where
    contramap f = Move . contramap f . unMove

instance (Functor m, TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
    Applicative (Move xs x r s e m)
  where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕

type Atom :: Action

-- | Fails noncommittally; see 'try'
newtype Atom xs x r s e m a = Atom{ unAtom :: Query xs x r s e m (Sure xs x r s e m a) }
    deriving stock Functor

instance Returnable Atom where
    trivial = Atom . trivial . trivial

instance BasicAction Atom

instance Functor m => Contravariant (Atom xs x r s e m a) (Atom xs x r' s e m a) r r' where
    contramap f = Atom . contramap f . fmap (contramap f) . unAtom

instance (Functor m, TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom xs x r s e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕

type AtomicMove :: Action

-- | Always moves the cursor, is atomic
newtype AtomicMove xs x r s e m a = AtomicMove{ unAtomicMove :: Atom xs x r s e m a }
    deriving stock Functor

instance BasicAction AtomicMove

instance Functor m => Contravariant (AtomicMove xs x r s e m a) (AtomicMove xs x r' s e m a) r r' where
    contramap f = AtomicMove . contramap f . unAtomicMove

instance (Functor m, TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove xs x r s e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕

type Sure :: Action

-- | Always succeeds
newtype Sure xs x r s e m a = Sure{ unSure :: Walk 'ReadWrite 'Perfect xs x r s e m a }
    deriving newtype (Functor, Applicative, Monad, Returnable, BasicAction, MonadicAction)

instance Functor m => Contravariant (Sure xs x r s e m a) (Sure xs x r' s e m a) r r' where
    contramap f = Sure . contramap f . unSure

-- ⭕

type SureQuery :: Action

-- | Always succeeds, does not move the cursor
newtype SureQuery xs x r s e m a = SureQuery{ unSureQuery :: Walk 'ReadOnly 'Perfect xs x r s e m a }
    deriving newtype (Functor, Applicative, Monad, BasicAction, MonadicAction, Returnable)

instance Functor m => Contravariant (SureQuery xs x r s e m a) (SureQuery xs x r' s e m a) r r' where
    contramap f = SureQuery . contramap f . unSureQuery

-- ⭕

type Fail :: Action

-- | Never succeeds, never moves the cursor, never does anything at all
data Fail xs x r s e m a = Fail{ unFail :: r -> e }
    deriving stock Functor

instance BasicAction Fail

instance Functor m => Contravariant (Fail xs x r s e m a) (Fail xs x r' s e m a) r r' where
    contramap f = Fail . (. f) . unFail

instance Fallible Fail where
    mapError f (Fail x) = Fail \r -> f (x r)

reFail :: Fail xs x r s e m a -> Fail xs x r s e m a'
reFail (Fail x) = Fail x

-- ⭕

class Is (act1 :: Action) (act2 :: Action) where
    cast :: Functor m => act1 xs x r s e m a -> act2 xs x r s e m a

castTo :: forall act2 act1 xs x r s e m a. (Functor m, Is act1 act2) =>
    act1 xs x r s e m a -> act2 xs x r s e m a
castTo = cast @act1 @act2

-- ⭕ Everything is itself

instance {-# overlappable #-} Is a a where
    cast = id

-- ⭕ Casting between types of Step

instance Is (Step 'ReadOnly p) (Step 'ReadWrite p) where
    cast = \case
        Base_Commit x -> case x of {}
        Base_RST x -> Base_RST x
        Base_Next x -> Base_Next x
        Base_Reset x -> Base_Reset x
        Base_Fail x -> Base_Fail x

instance Is (Step mo 'Perfect) (Step mo 'Imperfect) where
    cast = \case
        Base_Fail x -> case x of {}
        Base_RST x -> Base_RST x
        Base_Next x -> Base_Next x
        Base_Reset x -> Base_Reset x
        Base_Commit x -> Base_Commit x

instance Is (Step 'ReadOnly 'Perfect) (Step 'ReadWrite 'Imperfect) where
    cast = \case
        Base_Fail x -> case x of {}
        Base_Commit x -> case x of {}
        Base_RST x -> Base_RST x
        Base_Next x -> Base_Next x
        Base_Reset x -> Base_Reset x

-- ⭕ Straightforward casting from Step to Walk via lifting into Free

instance Is (Step 'ReadOnly 'Perfect) SureQuery where
    cast = SureQuery . Walk . liftF

instance Is (Step 'ReadOnly 'Imperfect) Query where
    cast = Query . Walk . liftF

instance Is (Step 'ReadWrite 'Perfect) Sure where
    cast = Sure . Walk . liftF

instance Is (Step 'ReadWrite 'Imperfect) Any where
    cast = Any . Walk . liftF

-- ⭕ Two-step casts that involve casting between Step types and then into Free

instance Is (Step 'ReadOnly 'Perfect) Sure where
    cast = Sure . Walk . liftF . cast

instance Is (Step 'ReadOnly 'Perfect) Query where
    cast = Query . Walk . liftF . cast

instance Is (Step 'ReadOnly 'Imperfect) Any where
    cast = Any . Walk . liftF . cast

instance Is (Step 'ReadWrite 'Perfect) Any where
    cast = Any . Walk . liftF . cast

-- ⭕ Casting out of read-only

instance Is SureQuery Sure where
    cast = Sure . Walk . hoistFree cast . unWalk . unSureQuery

instance Is Query Any where
    cast = Any . Walk . hoistFree cast . unWalk . unQuery

-- ⭕ Casting out of sureness

instance Is SureQuery Query where
    cast = Query . Walk . hoistFree cast . unWalk . unSureQuery

instance Is Sure Any where
    cast = Any . Walk . hoistFree cast . unWalk . unSure

-- ⭕ Casting out of both read-only and sureness

instance Is SureQuery Any where
    cast = Any . Walk . hoistFree cast . unWalk . unSureQuery

-- ⭕ Casting to Atom

instance Is SureQuery Atom where
    cast = Atom . fmap return . castTo @Query

instance Is Query Atom where
    cast = Atom . fmap return

instance Is Sure Atom where
    cast = Atom . return

-- ⭕ Casting out of atomicity

instance Is Atom Any where
    cast = Monad.join . fmap (cast @Sure @Any) . cast @Query @Any . unAtom

instance Is AtomicMove Move where
    cast = Move . cast @Atom @Any . unAtomicMove

-- ⭕ Casting out of movement

instance Is Move Any where
    cast = unMove

instance Is AtomicMove Atom where
    cast = unAtomicMove

-- ⭕ Casting out of both atomicity and movement

instance Is AtomicMove Any where
    cast = cast . castTo @Move

-- ⭕ Casting out of fail

instance Is Fail Any where
    cast = Any . Walk . liftF . Base_Fail

instance Is Fail Query where
    cast = Query . Walk . liftF . Base_Fail

instance Is Fail Move where
    cast = Move . Any . Walk . liftF . Base_Fail

instance Is Fail Atom where
    cast = Atom . Query . Walk . liftF . Base_Fail . reFail

instance Is Fail AtomicMove where
    cast = AtomicMove . Atom . Query . Walk . liftF . Base_Fail . reFail

-- ⭕

class Is act1 act2 => LossOfMovement act1 act2 | act1 -> act2

instance LossOfMovement Any Any

instance LossOfMovement Atom Atom

instance LossOfMovement Sure Sure

instance LossOfMovement Query Query

instance LossOfMovement Move Any

instance LossOfMovement AtomicMove Atom

instance LossOfMovement Fail Fail

instance LossOfMovement SureQuery SureQuery

-- ⭕

class Is act2 act1 => AssumeMovement act1 act2 | act1 -> act2 where
    assumeMovement :: act1 xs x r s e m a -> act2 xs x r s e m a

instance AssumeMovement Any Move where
    assumeMovement = Move

instance AssumeMovement Atom AtomicMove where
    assumeMovement = AtomicMove

instance AssumeMovement Move Move where
    assumeMovement = id

instance AssumeMovement AtomicMove AtomicMove where
    assumeMovement = id

-- ⭕

class Is act2 act1 => AssumeSuccess act1 act2 | act1 -> act2 where
    assumeSuccess :: Monad m => act1 xs x r s e m a -> act2 xs x r s e m a

instance AssumeSuccess (Step 'ReadOnly 'Imperfect) (Step 'ReadOnly 'Perfect) where
    assumeSuccess = \case
        Base_Commit x -> case x of {}
        Base_RST x -> Base_RST x
        Base_Next x -> Base_Next x
        Base_Reset x -> Base_Reset x
        Base_Fail _ -> error "assumeSuccess: assumption failed"

instance AssumeSuccess (Step 'ReadWrite 'Imperfect) (Step 'ReadWrite 'Perfect) where
    assumeSuccess = \case
        Base_Commit x -> Base_Commit x
        Base_RST x -> Base_RST x
        Base_Next x -> Base_Next x
        Base_Reset x -> Base_Reset x
        Base_Fail _ -> error "assumeSuccess: assumption failed"

instance AssumeSuccess Any Sure where
    assumeSuccess (Any (Walk x)) = Sure (Walk (hoistFree assumeSuccess x))

instance AssumeSuccess Query SureQuery where
    assumeSuccess (Query (Walk x)) = SureQuery (Walk (hoistFree assumeSuccess x))

-- ⭕

class (BasicAction act, BasicAction try) => Atomic act try | act -> try where
    try :: Functor m => act xs x r s e m a -> try xs x r s e m (Maybe a)

instance Atomic Atom Sure where
    try (Atom x) = castTo @Sure (try @Query x) >>= maybe (return Nothing) (fmap Just)

instance Atomic AtomicMove Sure where
    try = try @Atom . castTo @Atom

instance Atomic Query SureQuery where
    try :: forall xs x r s e m a. Functor m => Query xs x r s e m a -> SureQuery xs x r s e m (Maybe a)
    try (Query (Walk q)) = SureQuery (Walk (freeTryR q))

freeTryR :: Functor m => Free (Step 'ReadOnly 'Imperfect xs x r s e m) a -> Free (Step 'ReadOnly 'Perfect xs x r s e m) (Maybe a)
freeTryR = \case
    Pure x -> return (Just x)
    Free b -> case b of
        Base_Fail _ -> return Nothing
        Base_RST x -> liftF (Base_RST x) >>= freeTryR
        Base_Reset x -> liftF (Base_Reset x) >>= freeTryR
        Base_Next x -> liftF (Base_Next x) >>= freeTryR
        Base_Commit x -> case x of {}

-- ⭕

-- | Loop0 act1 act2 means that a repetition of 0 or more act1 actions results in an act2 action.
class (Join act1 act2, Returnable act2, Is (act1 >> act2) act2) =>
    Loop0 act1 act2 | act1 -> act2

-- Atomic actions loose their atomicity when sequenced 2 or more times; guaranteed advancement is lost when sequencing 0 times

instance Loop0 Atom Any
instance Loop0 AtomicMove Any
instance Loop0 Move Any

-- Other kinds are preserved

instance Loop0 Any Any
instance Loop0 Sure Sure
instance Loop0 SureQuery SureQuery
instance Loop0 Query Query

-- ⭕

-- | Loop1 act1 act2 means that a repetition of 1 or more act1 actions results in an act2 action.
class (Join act1 act2, Is act1 act2, Is (act1 >> act2) act2) =>
    Loop1 act1 act2 | act1 -> act2

-- Atomic actions loose their atomicity when sequenced 2 or more times

instance Loop1 Atom Any
instance Loop1 AtomicMove Move

-- All other kinds are preserved by sequencing

instance Loop1 Any Any
instance Loop1 Query Query
instance Loop1 Move Move
instance Loop1 Sure Sure
instance Loop1 SureQuery SureQuery

-- ⭕

-- | The type @a >> b@ is type of the expression @a >> b@.
--
-- This function is mostly not commutative (@a >> b@ is not the same as @b >> a@) because whether an atomic action's atomicity is preserved depends on the order of the composition in some cases.

type family (act1 :: Action) >> (act2 :: Action) :: Action
  where

    -- When failure is first, the second step is irrelevant.
    Fail >> k = Fail

    -- When failure is second, sureness and atomicity are lost.
    Sure >> Fail = Any
    SureQuery >> Fail = Query
    Atom >> Fail = Any
    AtomicMove >> Fail = Move
    k >> Fail = k

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

-- ⭕

class (BasicAction act1, BasicAction act2, BasicAction (act1 >> act2)) => Join act1 act2 where
    join :: Functor m => act1 xs x r s e m (act2 xs x r s e m a) -> (act1 >> act2) xs x r s e m a

cast2 :: forall act2 act1 f xs x r s e m a. (Is act1 act2, Functor m, Functor f) => f (act1 xs x r s e m a) -> f (act2 xs x r s e m a)
cast2 = fmap castTo

-- ⭕

instance Join Any Any where
    join = Monad.join
instance Join Any Atom where
    join = join @Any @Any . cast2 @Any
instance Join Any AtomicMove where
    join = assumeMovement . join @Any @Any . cast2 @Any
instance Join Any Fail where
    join = join @Any @Any . cast2 @Any
instance Join Any Move where
    join = assumeMovement . join @Any @Any . cast2 @Any
instance Join Any Query where
    join = join @Any @Any . cast2 @Any
instance Join Any SureQuery where
    join = join @Any @Any . cast2 @Any
instance Join Any Sure where
    join = join @Any @Any . cast2 @Any

instance Join Atom Any where
    join = join @Any @Any . castTo @Any
instance Join Atom Atom where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Fail where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom SureQuery where
    join = Atom . fmap (join @Sure @SureQuery) . (\(Atom q) -> q)
instance Join Atom Sure where
    join = Atom . fmap (join @Sure @Sure) . (\(Atom q) -> q)

instance Join AtomicMove Any where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Atom where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove AtomicMove where
    join = join . castTo @Atom
instance Join AtomicMove Fail where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Move where
    join = join . castTo @Atom
instance Join AtomicMove Query where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove SureQuery where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Sure where
    join = assumeMovement . join . castTo @Atom

instance Join Fail Any where
    join (Fail f) = Fail f
instance Join Fail Atom where
    join (Fail f) = Fail f
instance Join Fail AtomicMove where
    join (Fail f) = Fail f
instance Join Fail Fail where
    join (Fail f) = Fail f
instance Join Fail Move where
    join (Fail f) = Fail f
instance Join Fail Query where
    join (Fail f) = Fail f
instance Join Fail Sure where
    join (Fail f) = Fail f
instance Join Fail SureQuery where
    join (Fail f) = Fail f

instance Join Move Any where
    join = assumeMovement . join . castTo @Any
instance Join Move Atom where
    join = assumeMovement . join . castTo @Any
instance Join Move AtomicMove where
    join = join . castTo @Any
instance Join Move Fail where
    join = assumeMovement . join . castTo @Any
instance Join Move Move where
    join = join . castTo @Any
instance Join Move Query where
    join = assumeMovement . join . castTo @Any
instance Join Move SureQuery where
    join = assumeMovement . join . castTo @Any
instance Join Move Sure where
    join = assumeMovement . join . castTo @Any

instance Join Query Any where
    join = join @Any @Any . castTo @Any
instance Join Query Atom where
    join = Atom . join . fmap (\(Atom q) -> q)
instance Join Query AtomicMove where
    join = assumeMovement . join @Query @Atom . cast2 @Atom
instance Join Query Fail where
    join = join @Query @Query . cast2 @Query
instance Join Query Move where
    join = assumeMovement . join @Query @Any . cast2 @Any
instance Join Query Query where
    join = Monad.join
instance Join Query SureQuery where
    join = join @Query @Query . cast2 @Query
instance Join Query Sure where
    join = Atom

instance Join Sure Any where
    join = join @Any @Any . castTo @Any
instance Join Sure Atom where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Fail where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure SureQuery where
    join = join @Sure @Sure . cast2 @Sure
instance Join Sure Sure where
    join = Monad.join

instance Join SureQuery Any where
    join = join @Any @Any . castTo @Any
instance Join SureQuery Atom where
    join = Atom . join @SureQuery @Query . fmap (\(Atom q) -> q)
instance Join SureQuery AtomicMove where
    join = assumeMovement . join . cast2 @Atom
instance Join SureQuery Fail where
    join = join @Query @Fail . castTo @Query
instance Join SureQuery Move where
    join = assumeMovement . join. cast2 @Any
instance Join SureQuery Query where
    join = join @Query @Query . castTo @Query
instance Join SureQuery Sure where
    join = join @Sure @Sure . castTo @Sure
instance Join SureQuery SureQuery where
    join = Monad.join

-- ⭕

infixl 1 `bindAction`
bindAction :: Functor m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x r s e m a -> (a -> act2 xs x r s e m b) -> act3 xs x r s e m b
bindAction x f = join (fmap f x)

-- ⭕

commit :: Monad m => Positive Natural -> AtomicMove xs x r s e m ()
commit n = AtomicMove $ Atom $ Query $ return $ Sure $ Walk $ liftF $ Base_Commit $ Commit n ()

fail :: Fail xs x r s r m a
fail = Fail id

takeCharMaybe :: Monad m => Sure xs x r s r m (Maybe x)
takeCharMaybe = try takeChar

takeChar :: Monad m => AtomicMove xs x r s r m x
takeChar = nextChar `bindAction` \x -> commit one $> x

nextChar :: Monad m => Query xs x r s r m x
nextChar = nextCharMaybe `bindAction` maybe (castTo @Query fail) return

nextMaybe :: Monad m => SureQuery xs x r s e m (Maybe (Nontrivial xs x))
nextMaybe = reset `bindAction` \() -> nextMaybe'

-- | Like 'nextMaybe', but doesn't reset first
nextMaybe' :: Functor m => SureQuery xs x r s e m (Maybe (Nontrivial xs x))
nextMaybe' = cast $ Base_Next @'ReadOnly @'Perfect id

next :: Monad m => Query xs x r s r m (Nontrivial xs x)
next = nextMaybe `bindAction` maybe (castTo @Query fail) return

-- | Like 'next', but doesn't reset first
next' :: Monad m => Query xs x r s r m (Nontrivial xs x)
next' = nextMaybe' `bindAction` maybe (castTo @Query fail) return

takeNext :: Monad m => AtomicMove xs x r s r m (Nontrivial xs x)
takeNext = next `bindAction` \xs -> commit (Nontrivial.length xs) $> xs

takeNextMaybe :: Monad m => Sure xs x r s r m (Maybe (Nontrivial xs x))
takeNextMaybe = try takeNext

nextCharMaybe :: Monad m => SureQuery xs x r s e m (Maybe x)
nextCharMaybe = nextMaybe <&> fmap @Maybe Nontrivial.head

satisfyJust :: Monad m => (x -> Maybe a) -> AtomicMove xs x r s r m a
satisfyJust ok = nextCharMaybe `bindAction` \x -> case x >>= ok of Nothing -> castTo fail; Just y -> commit one $> y

skip0 :: Monad m => Natural -> Any xs x r s r m ()
skip0 = maybe (return ()) (castTo @Any . skip)  . preview Positive.refine

skip :: Monad m => Positive Natural -> Move xs x r s r m ()
skip n = next `bindAction` \x ->
    case Positive.minus (Nontrivial.length x) n of
        Signed.Minus n' ->
            commit (Nontrivial.length x) `bindAction` \() -> skip n'
        _ -> castTo @Move (commit n)

skipAtomically0 :: Monad m => Natural -> Atom xs x r s r m ()
skipAtomically0 = maybe (trivial ()) (castTo @Atom . skipAtomically)  . preview Positive.refine

skipAtomically :: Monad m => Positive Natural -> AtomicMove xs x r s r m ()
skipAtomically n = ensureAtLeast n `bindAction` \() -> commit n

ensureAtLeast :: Monad m => Positive Natural -> Query xs x r s r m ()
ensureAtLeast = \n -> castTo @Query reset `bindAction` \() -> go n
  where
    go :: Monad m => Positive Natural -> Query xs x r s r m ()
    go n = next' `bindAction` \x ->
        case Positive.minus n (Nontrivial.length x) of
            Signed.Plus n' -> go n'
            _ -> return ()

atEnd :: Monad m => SureQuery xs x r s e m Bool
atEnd = reset `bindAction` \() -> nextMaybe' <&> isNothing

end :: Monad m => Query xs x r s r m ()
end = atEnd `bindAction` \e -> if e then trivial () else castTo @Query fail

reset :: Monad m => SureQuery xs x r s e m ()
reset = cast $ Base_Reset @'ReadOnly @'Perfect ()

actionState :: Monad m => SureQuery xs x r s e m s
actionState = cast $ Base_RST @'ReadOnly @'Perfect get

actionContext :: Monad m => SureQuery xs x r s e m r
actionContext = cast $ Base_RST @'ReadOnly @'Perfect ask

one :: Positive Natural
one = PositiveUnsafe 1

-- while :: Monad m => LossOfMovement act1 act2 => Nontrivial.GeneralSpanOperation xs x
--     -> act1 xs x r s e m a -> act2 xs x r s e m a
-- while = _


-- todo: add an atomic version of 'text'

-- text :: Nontrivial xs x -> Move xs x r s m ()
-- text = someOfNontrivialText A.>=> (maybe (return ()) (castTo @Any . text) . Nontrivial.refine)
--   where
--     someOfNontrivialText x = Action.Unsafe.AtomicMove $ case curse of
--         CursorRW{ init, input, commit } -> run $ Cursor.next input >>= \case
--             Nothing -> return (Left F.failure)
--             Just y ->
--                 if x `Nontrivial.isPrefixOf` y
--                 then commit (Nontrivial.length x) $> Right ListLike.empty
--                 else
--                 if y `Nontrivial.isPrefixOf` x
--                 then commit (Nontrivial.length y) $>
--                       Right
--                         (
--                           ListLike.drop
--                               (ListLike.length (Nontrivial.generalize y))
--                               (Nontrivial.generalize x)
--                         )
--                 else return (Left F.failure)

count0 :: forall act1 act2 xs x r s e m a. Monad m =>
    Loop0 act1 act2 => Natural -> act1 xs x r s e m a -> act2 xs x r s e m [a]
count0 = \n a -> go a n
  where
    go a = fix \r -> \case
        0 -> trivial []
        n -> castTo @act2 $
            a `bindAction` \x -> r (n - 1) <&> \xs -> x : xs

count1 :: forall act1 act2 xs x r s e m a. Monad m => Loop1 act1 act2 =>
    Positive Natural -> act1 xs x r s e m a -> act2 xs x r s e m (NonEmpty a)
count1 = \n a -> go a n
  where
    go a = fix \r -> \p ->
        case preview positive (review positive p - 1) of
            Nothing -> (:| []) <$> castTo @act2 a
            Just p' -> castTo @act2 $
                a `bindAction` \x -> r p' <&> \xs -> NonEmpty.cons x xs

repetition0 :: Monad m => AtomicMove xs x r s e m a -> Sure xs x r s e m [a]
repetition0 p = fix \r ->
    try p `bindAction` \case
        Nothing -> return []
        Just x -> (x :) <$> r

repetition1 :: Monad m => AtomicMove xs x r s e m a -> AtomicMove xs x r s e m (NonEmpty a)
repetition1 p = p `bindAction` \x -> repetition0 p <&> \xs -> x :| xs

-- ⭕

newtype CursorPosition = CursorPosition{ cursorPositionNatural :: Natural }
    deriving newtype (Eq, Ord, Show, Num)

strictlyIncreaseCursorPosition :: Positive Natural -> Endo CursorPosition
strictlyIncreaseCursorPosition = increaseCursorPosition . review Positive.refine

increaseCursorPosition :: Natural -> Endo CursorPosition
increaseCursorPosition x = Endo $ CursorPosition . (+ x) . cursorPositionNatural
