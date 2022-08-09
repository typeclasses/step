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

class FunctorialAction act => Returnable (act :: Action) where
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

-- type Joining    :: Action -> Action

-- data Joining act xs x r s e m a =
--     Plain (act xs x r s e m a)
--   | Join (act xs x r s e m (act xs x r s e m a))
--     deriving stock Functor

-- instance Returnable act => Returnable (Joining act) where trivial = Plain . trivial
-- instance FunctorialAction act => FunctorialAction (Joining act)
-- instance MonadicAction act => MonadicAction (Joining act)
-- instance (Returnable act, Monad m) => Applicative (Joining act xs x r s e m) where pure = return; (<*>) = ap
-- instance (Returnable act, Monad m) => Monad (Joining act xs x r s e m) where
--     return = trivial
--     a >>= f = case a of
--         Plain x -> _ (_ . f <$> x)

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

-- | The most general of the actions
data Any xs x r s e m a =
    Any_Base (Base xs x r s e m a)
  | Any_Commit (Positive Natural) a
  | Any_Join (Any xs x r s e m (Any xs x r s e m a))
    deriving stock Functor
    deriving anyclass (FunctorialAction, MonadicAction)

instance Returnable Any where trivial = Any_Base . Base_Lift . return
instance Monad m => Applicative (Any xs x r s e m) where pure = return; (<*>) = ap
instance Monad m => Monad (Any xs x r s e m) where return = trivial; a >>= f = Any_Join (fmap f a)

instance ContravariantAction Any where
    contramapAction f = \case
        Any_Base x -> Any_Base (contramapAction f x)
        Any_Join x -> Any_Join (contramapAction f (fmap (contramapAction f) x))
        x -> x

instance Fallible Any where
    mapError :: forall e e' xs x r s m a. Functor m =>
        (e -> e') -> Any xs x r s e m a -> Any xs x r s e' m a
    mapError f = go
      where
        go :: forall a'. Any xs x r s e m a' -> Any xs x r s e' m a'
        go = \case
            Any_Join x -> Any_Join (go (fmap go x))
            Any_Base x -> Any_Base (mapError f x)
            Any_Commit n x -> Any_Commit n x

---

-- | Does not move the cursor
data Query xs x r s e m a =
    Query_Base (Base xs x r s e m a)
  | Query_Join (Query xs x r s e m (Query xs x r s e m a))
    deriving stock Functor
    deriving anyclass (FunctorialAction, MonadicAction)

instance Returnable Query where trivial = Query_Base . Base_Lift . return
instance Monad m => Applicative (Query xs x r s e m) where pure = return; (<*>) = ap
instance Monad m => Monad (Query xs x r s e m) where return = trivial; a >>= f = Query_Join (fmap f a)

instance Fallible Query where
    mapError :: forall e e' xs x r s m a. Functor m =>
        (e -> e') -> Query xs x r s e m a -> Query xs x r s e' m a
    mapError f = go
      where
        go :: forall a'. Query xs x r s e m a' -> Query xs x r s e' m a'
        go = \case
            Query_Join x -> Query_Join (go (fmap go x))
            Query_Base x -> Query_Base (mapError f x)

instance ContravariantAction Query where
    contramapAction f = \case
        Query_Base x -> Query_Base (contramapAction f x)
        Query_Join x -> Query_Join (contramapAction f (fmap (contramapAction f) x))

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
