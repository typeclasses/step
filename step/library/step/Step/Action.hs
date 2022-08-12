{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving, EmptyCase #-}

module Step.Action where

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

{- $subtypes

Arrows in the graph below indicate permitted use of 'cast'. (Not pictured: 'Fail')

![Action subtyping graph](graphics/action-subtyping.svg)

-}



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

class (forall xs x r s e m. Functor m => Functor (act xs x r s e m)) => FunctorialAction (act :: Action)

class Returnable (act :: Action) where
    trivial :: a -> act xs x r s e m a

class (FunctorialAction act, Returnable act, forall xs x r s e m. Functor m => Monad (act xs x r s e m)) =>
    MonadicAction (act :: Action)

class ContravariantAction (act :: Action) where
    contramapAction :: Functor m => (r -> r) -> act xs x r s e m a -> act xs x r s e m a

class Fallible (act :: Action) where
    mapError :: Functor m => (e -> e') -> act xs x r s e m a -> act xs x r s e' m a

class Infallible (act :: Action) where
    mapError' :: Functor m => act xs x r s e m a -> act xs x r s e' m a

-- class IsFree (base :: Action) (act :: Action) | act -> base where
--     freeIso :: Iso (act xs x r s e m a) (act xs x r s e' m a) (Free (base xs x r s e m) a) (Free (base xs x r s e' m) a)



data Mode = ReadWrite | ReadOnly

data family Commit (m :: Mode) :: Type -> Type

data instance Commit 'ReadOnly a
    deriving stock Functor

data instance Commit 'ReadWrite a = Commit (Positive Natural) a
    deriving stock Functor



data Perfection = Perfect | Imperfect

type family Error (p :: Perfection) (e :: Type) :: Type

type instance Error 'Perfect e = Void

type instance Error 'Imperfect e = e



type Base :: Mode -> Perfection -> Action

data Base (mo :: Mode) (p :: Perfection) xs x r s e m a =
    Base_RST (RST r s m a)
  | Base_Reset a
  | Base_Next (Maybe (Nontrivial xs x) -> a)
  | Base_Fail (Error p (Fail xs x r s e m a))
  | Base_Commit (Commit mo a)

instance Functor m => Functor (Base 'ReadOnly 'Perfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (f . x)
        Base_Fail x -> case x of {}
        Base_Commit x -> case x of {}

instance Functor m => Functor (Base 'ReadOnly 'Imperfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (fmap f x)
        Base_Fail x -> Base_Fail (fmap f x)
        Base_Commit x -> case x of {}

instance Functor m => Functor (Base 'ReadWrite 'Perfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (f . x)
        Base_Fail x -> case x of {}
        Base_Commit x -> Base_Commit (fmap f x)

instance Functor m => Functor (Base 'ReadWrite 'Imperfect xs x r s e m) where
    fmap f = \case
        Base_RST x -> Base_RST (fmap f x)
        Base_Reset x -> Base_Reset (f x)
        Base_Next x -> Base_Next (f . x)
        Base_Fail x -> Base_Fail (fmap f x)
        Base_Commit x -> Base_Commit (fmap f x)

instance FunctorialAction (Base 'ReadOnly 'Perfect)
instance FunctorialAction (Base 'ReadOnly 'Imperfect)
instance FunctorialAction (Base 'ReadWrite 'Perfect)
instance FunctorialAction (Base 'ReadWrite 'Imperfect)

instance ContravariantAction (Base 'ReadOnly 'Perfect)

instance ContravariantAction (Base 'ReadOnly 'Imperfect)

instance ContravariantAction (Base 'ReadWrite 'Perfect)

instance ContravariantAction (Base 'ReadWrite 'Imperfect)

-- instance Fallible (Base mo p) where
--     mapError f = \case
--         Base_Fail g -> Base_Fail (mapError f g)
--         Base_RST x -> Base_RST x
--         Base_Reset x -> Base_Reset x
--         Base_Next x -> Base_Next x



type BaseM :: Mode -> Perfection -> Action

newtype BaseM mo p xs x r s e m a = BaseM (Free (Base mo p xs x r s e m) a)

deriving newtype instance Functor m => Functor (BaseM 'ReadWrite 'Perfect xs x r s e m)
deriving newtype instance Functor m => Functor (BaseM 'ReadWrite 'Imperfect xs x r s e m)
deriving newtype instance Functor m => Functor (BaseM 'ReadOnly 'Perfect xs x r s e m)
deriving newtype instance Functor m => Functor (BaseM 'ReadOnly 'Imperfect xs x r s e m)

deriving newtype instance Functor m => Applicative (BaseM 'ReadWrite 'Perfect xs x r s e m)
deriving newtype instance Functor m => Applicative (BaseM 'ReadWrite 'Imperfect xs x r s e m)
deriving newtype instance Functor m => Applicative (BaseM 'ReadOnly 'Perfect xs x r s e m)
deriving newtype instance Functor m => Applicative (BaseM 'ReadOnly 'Imperfect xs x r s e m)

deriving newtype instance Functor m => Monad (BaseM 'ReadWrite 'Perfect xs x r s e m)
deriving newtype instance Functor m => Monad (BaseM 'ReadWrite 'Imperfect xs x r s e m)
deriving newtype instance Functor m => Monad (BaseM 'ReadOnly 'Perfect xs x r s e m)
deriving newtype instance Functor m => Monad (BaseM 'ReadOnly 'Imperfect xs x r s e m)

-- instance IsFree act (BaseM act) where
--     freeIso = coerced

instance FunctorialAction (BaseM 'ReadWrite 'Perfect)
instance FunctorialAction (BaseM 'ReadWrite 'Imperfect)
instance FunctorialAction (BaseM 'ReadOnly 'Perfect)
instance FunctorialAction (BaseM 'ReadOnly 'Imperfect)

instance MonadicAction (BaseM 'ReadWrite 'Perfect)
instance MonadicAction (BaseM 'ReadWrite 'Imperfect)
instance MonadicAction (BaseM 'ReadOnly 'Perfect)
instance MonadicAction (BaseM 'ReadOnly 'Imperfect)

instance Returnable (BaseM mo p) where
    trivial = BaseM . Pure

instance ContravariantAction (BaseM 'ReadWrite 'Perfect)
instance ContravariantAction (BaseM 'ReadWrite 'Imperfect)
instance ContravariantAction (BaseM 'ReadOnly 'Perfect)
instance ContravariantAction (BaseM 'ReadOnly 'Imperfect)

    -- contramapAction f (BaseM x) = BaseM (hoistFree (contramapAction f) x)

-- instance (FunctorialAction act, Fallible act) => Fallible (BaseM mo p) where
--     mapError f = over freeIso $ hoistFree $ mapError f

-- instance (FunctorialAction act, Infallible act) => Infallible (BaseM mo p) where
--     mapError' = over freeIso $ hoistFree mapError'



type Any :: Action

-- | The most general of the actions; a monadic combination of 'BaseRW'
newtype Any xs x r s e m a = Any (BaseM 'ReadWrite 'Imperfect xs x r s e m a)
    deriving newtype (Functor, Applicative, Monad, Returnable, ContravariantAction, FunctorialAction, MonadicAction)

-- instance IsFree (Base 'ReadWrite 'Imperfect) Any where freeIso = coerced



type Query :: Action

-- | Like 'Any', but cannot move the cursor; a monadic combination of 'Base'
newtype Query xs x r s e m a = Query (BaseM 'ReadOnly 'Imperfect xs x r s e m a)
    deriving newtype (FunctorialAction, MonadicAction, Returnable, Functor, Applicative, Monad, ContravariantAction)

-- instance IsFree BaseR Query where freeIso = coerced



type Move :: Action

-- | Always moves the cursor
newtype Move xs x r s e m a = Move (Any xs x r s e m a)
    deriving newtype (Functor, FunctorialAction, ContravariantAction)

instance (Functor m, TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
    Applicative (Move xs x r s e m)
  where
    pure = error "unreachable"
    (<*>) = error "unreachable"



type Atom :: Action

-- | Fails noncommittally; see 'try'
newtype Atom xs x r s e m a = Atom (Query xs x r s e m (Sure xs x r s e m a))
    deriving stock Functor
    deriving anyclass FunctorialAction

instance Returnable Atom where
    trivial = Atom . trivial . trivial

instance ContravariantAction Atom where
    contramapAction f (Atom a) = Atom (contramapAction f (fmap (contramapAction f) a))

-- instance Fallible Atom where
--     mapError f (Atom q) = Atom (fmap mapError' $ mapError f q)

instance (Functor m, TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom xs x r s e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"



type AtomicMove :: Action

-- | Always moves the cursor, is atomic
newtype AtomicMove xs x r s e m a = AtomicMove (Atom xs x r s e m a)
    deriving stock Functor
    deriving anyclass FunctorialAction
    -- deriving newtype Fallible

instance ContravariantAction AtomicMove where contramapAction f (AtomicMove a) = AtomicMove (contramapAction @Atom f a)

instance (Functor m, TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove xs x r s e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"



type Sure :: Action

-- | Always succeeds
newtype Sure xs x r s e m a = Sure (BaseM 'ReadWrite 'Perfect xs x r s e m a)
    deriving newtype (Functor, Applicative, Monad, Returnable, ContravariantAction, FunctorialAction, MonadicAction)

-- instance IsFree SureBaseRW Sure where freeIso = coerced



type SureQuery :: Action

-- | Always succeeds, does not move the cursor
newtype SureQuery xs x r s e m a = SureQuery (BaseM 'ReadOnly 'Perfect xs x r s e m a)
    deriving newtype (Functor, Applicative, Monad, ContravariantAction, FunctorialAction, MonadicAction, Returnable)

-- instance IsFree SureBaseR SureQuery where freeIso = coerced



type Fail :: Action

-- | Never succeeds, never moves the cursor, never does anything at all
data Fail xs x r s e m a = Fail (r -> e)
    deriving stock Functor
    deriving anyclass FunctorialAction

instance ContravariantAction Fail where contramapAction f (Fail g) = Fail (g . f)
instance Fallible Fail where mapError f (Fail x) = Fail \r -> f (x r)



class Is (act1 :: Action) (act2 :: Action) where
    cast :: Functor m => act1 xs x r s e m a -> act2 xs x r s e m a

castTo :: forall act2 act1 xs x r s e m a. (Functor m, Is act1 act2) =>
    act1 xs x r s e m a -> act2 xs x r s e m a
castTo = cast @act1 @act2

-- instance FunctorialAction act => Is act (FreeAction act) where cast = FreeAction . liftF

-- castBaseM :: Is (Base mo1 p1) (Base mo2 p2) => Functor m => BaseM mo1 p1 xs x r s e m a -> BaseM mo2 p2 xs x r s e m a
-- castBaseM (BaseM x) = BaseM (hoistFree cast x)

-- Everything is itself

instance Is Any Any where
    cast = id

instance Is (Base mo p) (Base mo p) where
    cast = id

instance Is Query Query where
    cast = id

instance Is Move Move where
    cast = id

instance Is Atom Atom where
    cast = id

instance Is AtomicMove AtomicMove where
    cast = id

instance Is Sure Sure where
    cast = id

instance Is SureQuery SureQuery where
    cast = id

instance Is Fail Fail where
    cast = id

-- SureBaseR is everything but Move, Fail

instance Is (Base 'ReadOnly 'Perfect) Any where
    -- cast = Any . FreeAction . liftF . castTo @BaseRW

instance Is (Base 'ReadOnly 'Perfect) (Base 'ReadOnly 'Imperfect) where
    -- cast = mapError absurd . (\(SureBaseRW x) -> x)

instance Is (Base 'ReadOnly 'Perfect) Query where
    -- cast = castTo @Query . castTo @Base

instance Is (Base 'ReadOnly 'Perfect) Atom where
    -- cast a = Atom (castTo @Query (castTo @Base a) <&> return)

instance Is (Base 'ReadOnly 'Perfect) Sure where
    -- cast = Sure . FreeAction . liftF . castTo @SureBaseRW

instance Is (Base 'ReadOnly 'Perfect) SureQuery where
    -- cast = SureQuery . cast

instance Is (Base 'ReadOnly 'Perfect) (Base 'ReadWrite 'Imperfect) where
    -- cast = cast . castTo @SureBaseRW

instance Is (Base 'ReadOnly 'Perfect) (Base 'ReadWrite 'Perfect) where
    -- cast (SureBaseRW x) = SureBaseRW (cast x)

-- BaseR is everything but Sure, Move, Fail

instance Is (Base 'ReadOnly 'Imperfect) Any where
    cast = Any . BaseM . liftF . castTo @(Base 'ReadWrite 'Imperfect)

instance Is (Base 'ReadOnly 'Imperfect) Query where
    cast = Query . BaseM . liftF

instance Is (Base 'ReadOnly 'Imperfect) Atom where
    cast a = Atom (Query (BaseM (liftF a)) <&> return)

instance Is (Base 'ReadOnly 'Imperfect) (Base 'ReadWrite 'Imperfect) where
    -- cast = BaseRW_Base

-- BaseRW is...

instance Is (Base 'ReadWrite 'Imperfect) Atom where
    -- cast x = \case
    --     -- BaseRW_Commit n x -> Atom $ return $ Sure $ FreeAction $ liftF $ SureBaseRW (BaseRW_Commit n x)
    --     BaseRW x -> Atom $ Query $ FreeAction (liftF x) <&> return

instance Is (Base 'ReadWrite 'Imperfect) Any where
    cast = Any . BaseM . liftF

-- SureBaseRW is...

instance Is (Base 'ReadWrite 'Perfect) (Base 'ReadWrite 'Imperfect) where
    -- cast (SureBaseRW x) = mapError absurd x

instance Is (Base 'ReadWrite 'Perfect) Atom where
    -- cast = cast . castTo @(Base 'ReadWrite 'Imperfect)

instance Is (Base 'ReadWrite 'Perfect) Any where
    -- cast = cast . castTo @(Base 'ReadWrite 'Imperfect)

-- Everything is Any

instance Is Move Any where
    cast (Move x) = x

instance Is AtomicMove Any where
    cast (AtomicMove x) = cast x

instance Is Sure Any where
    -- cast (Sure x) = Any (castFreeBase @SureBaseRW @BaseRW x)

instance Is SureQuery Any where
    -- cast (SureQuery x) = Any (castFreeBase @SureBaseR @BaseRW x)

instance Is Query Any where
    -- cast (Query x) = Any (castFreeBase @Base @BaseRW x)

instance Is Atom Any where
    -- cast (Atom (Query q)) = Any do
    --     Sure x <- castFreeBase @Base @BaseRW q
    --     castFreeBase @SureBaseRW @BaseRW x

-- Atom + Move = AtomicMove

instance Is AtomicMove Move where
    cast (AtomicMove x) = Move (cast @Atom @Any x)

instance Is AtomicMove Atom where
    cast (AtomicMove x) = x

-- Sure + Query = SureQuery

instance Is SureQuery Sure where
    -- cast (SureQuery x) = Sure (castFreeBase @SureBaseR @SureBaseRW x)

instance Is SureQuery Query where
    -- cast (SureQuery x) = Query (castFreeBase @SureBaseR @Base x)

-- Trivial subtypes of Atom

instance Is Query Atom where
    cast = Atom . fmap return

instance Is Sure Atom where
    cast = Atom . return

instance Is SureQuery Atom where
    cast = castTo @Atom . castTo @Query

-- Fail is anything but Sure

instance Is Fail (Base 'ReadOnly 'Imperfect) where
    cast = Base_Fail

instance Is Fail Any where
    -- cast = castTo @Any . castTo @(Base)

instance Is Fail Query where
    -- cast = castTo @Query . castTo @Base

instance Is Fail Move where
    -- cast = Move . castTo @Any . castTo @Base

instance Is Fail Atom where
    -- cast = castTo @Atom . castTo @Base

instance Is Fail AtomicMove where
    cast = AtomicMove . castTo @Atom



class Is act1 act2 => LossOfMovement act1 act2 | act1 -> act2

instance LossOfMovement Any Any

instance LossOfMovement Atom Atom

instance LossOfMovement Sure Sure

instance LossOfMovement Query Query

instance LossOfMovement Move Any

instance LossOfMovement AtomicMove Atom

instance LossOfMovement Fail Fail

instance LossOfMovement SureQuery SureQuery



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



class Is act2 act1 => AssumeSuccess act1 act2 | act1 -> act2 where
    assumeSuccess :: Monad m => act1 xs x r s e m a -> act2 xs x r s e m a

instance AssumeSuccess (Base 'ReadOnly 'Imperfect) (Base 'ReadOnly 'Perfect) where
    -- assumeSuccess = SureBase . mapError (\_ -> error "assumeSuccess: assumption failed")

instance AssumeSuccess (Base 'ReadWrite 'Imperfect) (Base 'ReadWrite 'Perfect) where
    -- assumeSuccess = SureBaseRW . mapError (\_ -> error "assumeSuccess: assumption failed")

instance AssumeSuccess Any Sure where
    assumeSuccess (Any (BaseM x)) = Sure (BaseM (hoistFree assumeSuccess x))

instance AssumeSuccess Query SureQuery where
    assumeSuccess (Query (BaseM x)) = SureQuery (BaseM (hoistFree assumeSuccess x))



class (FunctorialAction act, FunctorialAction try) =>
    Atomic (act :: Action) (try :: Action) | act -> try
  where
    try :: Functor m => act xs x r s e m a -> try xs x r s e m (Maybe a)

instance Atomic Atom Sure
  where
    try (Atom x) = castTo @Sure (try @Query x) >>= maybe (return Nothing) (fmap Just)

instance Atomic AtomicMove Sure
  where
    try = try @Atom . castTo @Atom

instance Atomic Query SureQuery
  where
    try :: forall xs x r s e m a. Functor m => Query xs x r s e m a -> SureQuery xs x r s e m (Maybe a)
    try (Query (BaseM q)) = SureQuery (BaseM (freeTryR q))

freeTryR :: Functor m => Free (Base 'ReadOnly 'Imperfect xs x r s e m) a -> Free (Base 'ReadOnly 'Perfect xs x r s e m) (Maybe a)
freeTryR = \case
    Pure x -> return (Just x)
    Free b -> case b of
        Base_Fail _ -> return Nothing
        Base_RST x -> liftF (Base_RST x) >>= freeTryR
        Base_Reset x -> liftF (Base_Reset x) >>= freeTryR
        Base_Next x -> liftF (Base_Next x) >>= freeTryR
        Base_Commit x -> case x of {}



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



class (FunctorialAction act1, FunctorialAction act2, FunctorialAction (act1 >> act2)) =>
    Join (act1 :: Action) (act2 :: Action)
  where
    join :: Functor m =>
        act1 xs x r s e m (act2 xs x r s e m a)
        -> (act1 >> act2) xs x r s e m a

cast2 :: forall act2 act1 f xs x r s e m a.
    (Is act1 act2, Functor m, Functor f) =>
    f (act1 xs x r s e m a) -> f (act2 xs x r s e m a)
cast2 = fmap castTo



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



infixl 1 `bindAction`
bindAction :: Functor m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x r s e m a -> (a -> act2 xs x r s e m b) -> act3 xs x r s e m b
bindAction x f = join (fmap f x)



commit :: Monad m => Positive Natural -> AtomicMove xs x r s e m ()
commit n = assumeMovement $ castTo @Atom $ Base_Commit @'ReadWrite @'Perfect (Commit n ())

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
