{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving #-}

-- | This module defines 'Action' and types of that kind.

module Step.ActionTypes.Constructors where

import Step.Internal.Prelude

import Step.Nontrivial

-- | The kind of all the action types in "Step.Action.Types"
type Action =
    Type              -- ^ @xs@ - text
    -> Type           -- ^ @x@ - char
    -> Type           -- ^ @r@ - reader context
    -> Type           -- ^ @s@ - state context
    -> Type           -- ^ @e@ - error
    -> (Type -> Type) -- ^ @m@ - monadic context
    -> Type           -- ^ @a@ - produced upon success
    -> Type

class (forall xs x r s e m. Functor m => Functor (act xs x r s e m)) => FunctorialAction (act :: Action)

class Returnable (act :: Action) where
    trivial :: Monad m => a -> act xs x r s e m a

class (FunctorialAction act, Returnable act, forall xs x r s e m. Monad m => Monad (act xs x r s e m)) =>
    MonadicAction (act :: Action)

class ContravariantAction (act :: Action) where
    contramapAction :: Monad m => (r -> r) -> act xs x r s e m a -> act xs x r s e m a

class Fallible (act :: Action) where
    mapError :: Functor m => (e -> e') -> act xs x r s e m a -> act xs x r s e' m a

class Infallible (act :: Action) where
    mapError' :: Functor m => act xs x r s e m a -> act xs x r s e' m a

type Any        :: Action
type Atom       :: Action
type AtomicMove :: Action
type Base       :: Action
type Fail       :: Action
type Move       :: Action
type Query      :: Action
type Sure       :: Action
type SureBase   :: Action
type SureQuery  :: Action

type Joining    :: Action -> Action

data Joining act xs x r s e m a =
    Plain (act xs x r s e m a)
  | Join (Joining act xs x r s e m (Joining act xs x r s e m a))
    deriving stock Functor

instance Returnable act => Returnable (Joining act) where trivial = Plain . trivial
instance FunctorialAction act => FunctorialAction (Joining act)
instance MonadicAction act => MonadicAction (Joining act)
instance (FunctorialAction act, Returnable act, Monad m) => Applicative (Joining act xs x r s e m) where pure = return; (<*>) = ap
instance (FunctorialAction act, Returnable act, Monad m) => Monad (Joining act xs x r s e m) where return = trivial; a >>= f = Join (fmap f a)

instance (FunctorialAction act, Fallible act) => Fallible (Joining act) where
    mapError :: forall e e' xs x r s m a. Functor m =>
        (e -> e') -> Joining act xs x r s e m a -> Joining act xs x r s e' m a
    mapError f = go
      where
        go :: forall a'. Joining act xs x r s e m a' -> Joining act xs x r s e' m a'
        go = \case
            Plain x -> Plain (mapError f x)
            Join x -> Join (go (fmap go x))

instance (FunctorialAction act, ContravariantAction act) => ContravariantAction (Joining act) where
    contramapAction f = \case
        Plain x -> Plain (contramapAction f x)
        Join x -> Join (contramapAction f (fmap (contramapAction f) x))

---

data Base xs x r s e m a =
    Base_Lift (m a)
  | Base_Ask (r -> a)
  | Base_Get (s -> a)
  | Base_Reset a
  | Base_Next (Maybe (Nontrivial xs x) -> a)
  | Base_Fail (r -> e)
    deriving stock Functor
    deriving anyclass FunctorialAction

instance Returnable Base where trivial = Base_Lift . return

instance ContravariantAction Base where
    contramapAction f = \case
        Base_Ask g -> Base_Ask (g . f)
        Base_Fail g -> Base_Fail (g . f)
        x -> x

instance Fallible Base where
    mapError f = \case
        Base_Fail g -> Base_Fail (f . g)
        Base_Lift x -> Base_Lift x
        Base_Ask x -> Base_Ask x
        Base_Get x -> Base_Get x
        Base_Reset x -> Base_Reset x
        Base_Next x -> Base_Next x

---

-- | Like 'Base' but never fails
newtype SureBase xs x r s e m a = SureBase (Base xs x r s Void m a)
    deriving stock Functor
    deriving anyclass FunctorialAction

instance Infallible SureBase where mapError' (SureBase x) = SureBase (mapError absurd x)

---

data BaseRW xs x r s e m a =
    BaseRW_Base (Base xs x r s e m a)
  | BaseRW_Commit (Positive Natural) a
  deriving stock Functor
  deriving anyclass FunctorialAction

instance Returnable BaseRW where trivial = BaseRW_Base . trivial
instance ContravariantAction BaseRW where contramapAction f = \case{ BaseRW_Base x -> BaseRW_Base (contramapAction f x); x -> x }
instance Fallible BaseRW where mapError f = \case{ BaseRW_Base x -> BaseRW_Base (mapError f x); BaseRW_Commit n x -> BaseRW_Commit n x }

---

newtype SureBaseRW xs x r s e m a = SureBaseRW (BaseRW xs x r s Void m a)
    deriving newtype (Functor)
    deriving anyclass FunctorialAction

instance ContravariantAction SureBaseRW where contramapAction f (SureBaseRW x) = SureBaseRW (contramapAction f x)
instance Returnable SureBaseRW where trivial = SureBaseRW . trivial

---

-- | The most general of the actions
newtype Any xs x r s e m a = Any (Joining BaseRW xs x r s e m a)
    deriving newtype (Functor, Applicative, Monad, FunctorialAction, MonadicAction, Returnable, ContravariantAction, Fallible)

---

-- | Does not move the cursor
newtype Query xs x r s e m a = Query (Joining Base xs x r s e m a)
    deriving newtype (FunctorialAction, MonadicAction, Returnable, Functor, Applicative, Monad, Fallible, ContravariantAction)

---

-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor
--
newtype Move xs x r s e m a = Move (Any xs x r s e m a)
    deriving stock Functor
    deriving anyclass FunctorialAction
    deriving newtype Fallible

instance ContravariantAction Move where
    contramapAction f (Move a) = Move (contramapAction @Any f a)

---

-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity
--
newtype Atom xs x r s e m a = Atom (Query xs x r s e m (Sure xs x r s e m a))
    deriving stock Functor
    deriving anyclass FunctorialAction

instance Returnable Atom where trivial = Atom . trivial . trivial
instance ContravariantAction Atom where contramapAction f (Atom a) = Atom (contramapAction f (fmap (contramapAction f) a))
instance Fallible Atom where mapError f (Atom q) = Atom (mapError f (fmap mapError' q))

---

-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor
--
newtype AtomicMove xs x r s e m a = AtomicMove (Atom xs x r s e m a)
    deriving stock Functor
    deriving anyclass FunctorialAction
    deriving newtype Fallible

instance ContravariantAction AtomicMove where contramapAction f (AtomicMove a) = AtomicMove (contramapAction @Atom f a)

---

-- | Always succeeds
newtype Sure xs x r s e m a = Sure (Any xs x r s Void m a)
    deriving newtype (Functor, Applicative, Monad)
    deriving anyclass (FunctorialAction, MonadicAction)

instance Returnable Sure where trivial = Sure . trivial
instance ContravariantAction Sure where contramapAction f (Sure x) = Sure (contramapAction f x)
instance Infallible Sure where mapError' (Sure x) = Sure (mapError absurd x)

---

-- | Always succeeds, does not move the cursor
newtype SureQuery xs x r s e m a = SureQuery (Query xs x r s Void m a)
    deriving newtype (Functor, Applicative, Monad)
    deriving anyclass (FunctorialAction, MonadicAction)

instance Returnable SureQuery where trivial = SureQuery . trivial
instance ContravariantAction SureQuery where contramapAction f (SureQuery x) = SureQuery (contramapAction f x)
instance Infallible SureQuery where mapError' (SureQuery x) = SureQuery (mapError absurd x)

---

-- | Never succeeds, never moves the cursor, never does anything at all
data Fail xs x r s e m a = Fail (r -> e)
    deriving stock Functor
    deriving anyclass FunctorialAction

instance ContravariantAction Fail where contramapAction f (Fail g) = Fail (g . f)
instance Fallible Fail where mapError f (Fail x) = Fail \r -> f (x r)
