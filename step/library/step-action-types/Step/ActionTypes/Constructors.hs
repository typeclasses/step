{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving #-}

-- | This module defines 'Action' and types of that kind.

module Step.ActionTypes.Constructors where

import Step.Internal.Prelude

import Step.Nontrivial

import TypeLits (TypeError, ErrorMessage (Text))

import Step.RST

import Optics (coerced)
import Coerce (coerce)

import qualified Monad

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

class IsFree (base :: Action) (act :: Action) | act -> base where
    freeIso :: Iso (act xs x r s e m a) (act xs x r s e' m a) (Free (base xs x r s e m) a) (Free (base xs x r s e' m) a)



type Base :: Action

data Base xs x r s e m a =
    Base_RST (RST r s m a)
  | Base_Reset a
  | Base_Next (Maybe (Nontrivial xs x) -> a)
  | Base_Fail (Fail xs x r s e m a)
    deriving stock Functor
    deriving anyclass FunctorialAction

instance ContravariantAction Base where
    contramapAction f = \case
        Base_RST a -> Base_RST (contramap f a)
        Base_Fail g -> Base_Fail (contramapAction f g)
        x -> x

instance Fallible Base where
    mapError f = \case
        Base_Fail g -> Base_Fail (mapError f g)
        Base_RST x -> Base_RST x
        Base_Reset x -> Base_Reset x
        Base_Next x -> Base_Next x



type SureBase :: Action

-- | Like 'Base' but never fails
newtype SureBase xs x r s e m a = SureBase (Base xs x r s Void m a)
    deriving stock Functor
    deriving anyclass FunctorialAction

instance Infallible SureBase where
    mapError' (SureBase x) = SureBase (mapError absurd x)

instance ContravariantAction SureBase where
    contramapAction f (SureBase x) = SureBase (contramapAction f x)



type BaseRW :: Action

-- | 'Base' plus a commit action
data BaseRW xs x r s e m a =
    BaseRW_Base (Base xs x r s e m a)
  | BaseRW_Commit (Positive Natural) a
  deriving stock Functor
  deriving anyclass FunctorialAction

instance ContravariantAction BaseRW where
    contramapAction f = \case{ BaseRW_Base x -> BaseRW_Base (contramapAction f x); x -> x }

instance Fallible BaseRW where
    mapError f = \case{ BaseRW_Base x -> BaseRW_Base (mapError f x); BaseRW_Commit n x -> BaseRW_Commit n x }



type SureBaseRW :: Action

-- | Like 'BaseRW' but never fails
newtype SureBaseRW xs x r s e m a = SureBaseRW (BaseRW xs x r s Void m a)
    deriving stock Functor
    deriving anyclass FunctorialAction

instance ContravariantAction SureBaseRW where
    contramapAction f (SureBaseRW x) = SureBaseRW (contramapAction f x)

instance Infallible SureBaseRW where
    mapError' (SureBaseRW x) = SureBaseRW (mapError absurd x)



type FreeAction :: Action -> Action

newtype FreeAction act xs x r s e m a = FreeAction (Free (act xs x r s e m) a)
    deriving newtype (Functor, Applicative, Monad)

instance IsFree act (FreeAction act) where
    freeIso = coerced

instance FunctorialAction act => FunctorialAction (FreeAction act)

instance FunctorialAction act => MonadicAction (FreeAction act)

instance Returnable (FreeAction act) where
    trivial = FreeAction . Pure

instance (FunctorialAction act, ContravariantAction act) => ContravariantAction (FreeAction act) where
    contramapAction f = over freeIso $ hoistFree $ contramapAction f

instance (FunctorialAction act, Fallible act) => Fallible (FreeAction act) where
    mapError f = over freeIso $ hoistFree $ mapError f

instance (FunctorialAction act, Infallible act) => Infallible (FreeAction act) where
    mapError' = over freeIso $ hoistFree mapError'



type Any :: Action

-- | The most general of the actions; a monadic combination of 'BaseRW'
newtype Any xs x r s e m a = Any (FreeAction BaseRW xs x r s e m a)
    deriving newtype (Functor, Applicative, Monad, Returnable, ContravariantAction, FunctorialAction, MonadicAction, Fallible)

instance IsFree BaseRW Any where freeIso = coerced



type Query :: Action

-- | Like 'Any', but cannot move the cursor; a monadic combination of 'Base'
newtype Query xs x r s e m a = Query (FreeAction Base xs x r s e m a)
    deriving newtype (FunctorialAction, MonadicAction, Returnable, Functor, Applicative, Monad, Fallible, ContravariantAction)

instance IsFree Base Query where freeIso = coerced



type Move :: Action

-- | Always moves the cursor
newtype Move xs x r s e m a = Move (Any xs x r s e m a)
    deriving newtype (Functor, Fallible, FunctorialAction, ContravariantAction)

instance (Functor m, TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
    Applicative (Move xs x r s e m)
  where
    pure = error "unreachable"
    (<*>) = error "unreachable"



type Atom :: Action

-- | Fails noncommittally
newtype Atom xs x r s e m a = Atom (Query xs x r s e m (Sure xs x r s e m a))
    deriving stock Functor
    deriving anyclass FunctorialAction

instance Returnable Atom where
    trivial = Atom . trivial . trivial

instance ContravariantAction Atom where
    contramapAction f (Atom a) = Atom (contramapAction f (fmap (contramapAction f) a))

instance Fallible Atom where
    mapError f (Atom q) = Atom (fmap mapError' $ mapError f q)

instance (Functor m, TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom xs x r s e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"



type AtomicMove :: Action

-- | Always moves the cursor, is atomic
newtype AtomicMove xs x r s e m a = AtomicMove (Atom xs x r s e m a)
    deriving stock Functor
    deriving anyclass FunctorialAction
    deriving newtype Fallible

instance ContravariantAction AtomicMove where contramapAction f (AtomicMove a) = AtomicMove (contramapAction @Atom f a)

instance (Functor m, TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove xs x r s e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"



type Sure :: Action

-- | Always succeeds
newtype Sure xs x r s e m a = Sure (FreeAction SureBaseRW xs x r s e m a)
    deriving newtype (Functor, Applicative, Monad, Returnable, ContravariantAction, FunctorialAction, MonadicAction, Infallible)

instance IsFree SureBaseRW Sure where freeIso = coerced



type SureQuery :: Action

-- | Always succeeds, does not move the cursor
newtype SureQuery xs x r s e m a = SureQuery (FreeAction SureBase xs x r s e m a)
    deriving newtype (Functor, Applicative, Monad, ContravariantAction, Infallible, FunctorialAction, MonadicAction)

instance Returnable SureQuery where trivial = SureQuery . trivial
instance IsFree SureBase SureQuery where freeIso = coerced



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

instance FunctorialAction act => Is act (FreeAction act) where cast = FreeAction . liftF

castFreeBase :: Is act1 act2 => Functor m => FunctorialAction act2 => FreeAction act1 xs x r s e m a -> FreeAction act2 xs x r s e m a
castFreeBase (FreeAction x) = FreeAction (hoistFree cast x)

-- Everything is itself

instance Is Any Any where
    cast = id

instance Is Base Base where
    cast = id

instance Is BaseRW BaseRW where
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

instance Is SureBase SureBase where
    cast = id

instance Is SureBaseRW SureBaseRW where
    cast = id

instance Is SureQuery SureQuery where
    cast = id

instance Is Fail Fail where
    cast = id

-- SureBase is everything but Move, Fail

instance Is SureBase Any where
    cast = Any . FreeAction . liftF . castTo @BaseRW

instance Is SureBase Base where
    cast = mapError absurd . (\(SureBase x) -> x)

instance Is SureBase Query where
    cast = castTo @Query . castTo @Base

instance Is SureBase Atom where
    cast a = Atom (castTo @Query (castTo @Base a) <&> return)

instance Is SureBase Sure where
    cast = Sure . FreeAction . liftF . castTo @SureBaseRW

instance Is SureBase SureQuery where
    cast = SureQuery . cast

instance Is SureBase BaseRW where
    cast = cast . castTo @SureBaseRW

instance Is SureBase SureBaseRW where
    cast (SureBase x) = SureBaseRW (cast x)

-- Base is everything but Sure, Move, Fail

instance Is Base Any where
    cast = Any . FreeAction . liftF . cast @Base @BaseRW

instance Is Base Query where
    cast = Query . FreeAction . liftF

instance Is Base Atom where
    cast a = Atom (Query (FreeAction (liftF a)) <&> return)

instance Is Base BaseRW where
    cast = BaseRW_Base

-- BaseRW is...

instance Is BaseRW Atom where
    cast = \case
        BaseRW_Commit n x -> Atom $ return $ Sure $ FreeAction $ liftF $ SureBaseRW (BaseRW_Commit n x)
        BaseRW_Base x -> Atom $ Query $ FreeAction (liftF x) <&> return

instance Is BaseRW Any where
    cast = Any . FreeAction . liftF

-- SureBaseRW is...

instance Is SureBaseRW BaseRW where
    cast (SureBaseRW x) = mapError absurd x

instance Is SureBaseRW Atom where
    cast = cast . castTo @BaseRW

instance Is SureBaseRW Any where
    cast = cast . castTo @BaseRW

-- Everything is Any

instance Is Move Any where
    cast (Move x) = x

instance Is AtomicMove Any where
    cast (AtomicMove x) = cast x

instance Is Sure Any where
    cast (Sure x) = Any (castFreeBase @SureBaseRW @BaseRW x)

instance Is SureQuery Any where
    cast (SureQuery x) = Any (castFreeBase @SureBase @BaseRW x)

instance Is Query Any where
    cast (Query x) = Any (castFreeBase @Base @BaseRW x)

instance Is Atom Any where
    cast (Atom (Query q)) = Any do
        Sure x <- castFreeBase @Base @BaseRW q
        castFreeBase @SureBaseRW @BaseRW x

-- Atom + Move = AtomicMove

instance Is AtomicMove Move where
    cast (AtomicMove x) = Move (cast @Atom @Any x)

instance Is AtomicMove Atom where
    cast (AtomicMove x) = x

-- Sure + Query = SureQuery

instance Is SureQuery Sure where
    cast (SureQuery x) = Sure (castFreeBase @SureBase @SureBaseRW x)

instance Is SureQuery Query where
    cast (SureQuery x) = Query (castFreeBase @SureBase @Base x)

-- Trivial subtypes of Atom

instance Is Query Atom where
    cast = Atom . fmap return

instance Is Sure Atom where
    cast = Atom . return

instance Is SureQuery Atom where
    cast = castTo @Atom . castTo @Query

-- Fail is anything but Sure

instance Is Fail Base where
    cast = Base_Fail

instance Is Fail Any where
    cast = castTo @Any . castTo @Base

instance Is Fail Query where
    cast = castTo @Query . castTo @Base

instance Is Fail Move where
    cast = Move . castTo @Any . castTo @Base

instance Is Fail Atom where
    cast = castTo @Atom . castTo @Base

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

instance AssumeSuccess Base SureBase where
    assumeSuccess = SureBase . mapError (\_ -> error "assumeSuccess: assumption failed")

instance AssumeSuccess BaseRW SureBaseRW where
    assumeSuccess = SureBaseRW . mapError (\_ -> error "assumeSuccess: assumption failed")

instance AssumeSuccess Any Sure where
    assumeSuccess (Any (FreeAction x)) = Sure (FreeAction (hoistFree assumeSuccess x))

instance AssumeSuccess Query SureQuery where
    assumeSuccess (Query (FreeAction x)) = SureQuery (FreeAction (hoistFree assumeSuccess x))



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
    try (Query (FreeAction q)) = SureQuery (FreeAction (freeTry q))

freeTry :: Functor m => Free (Base xs x r s e m) a -> Free (SureBase xs x r s e m) (Maybe a)
freeTry = \case
    Pure x -> return (Just x)
    Free b -> case b of
        Base_Fail _ -> return Nothing
        Base_RST x -> liftF (SureBase (Base_RST x)) >>= freeTry
        Base_Reset x -> liftF (SureBase (Base_Reset x)) >>= freeTry
        Base_Next x -> liftF (SureBase (Base_Next x)) >>= freeTry



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
    join :: Monad m =>
        act1 xs x r s e m (act2 xs x r s e m a)
        -> (act1 >> act2) xs x r s e m a

cast2 :: forall act2 act1 f xs x r s e m a.
    (Is act1 act2, Monad m, Functor f) =>
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
