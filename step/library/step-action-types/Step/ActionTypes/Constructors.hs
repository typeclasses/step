{-# language DataKinds, StandaloneKindSignatures, Unsafe #-}

{-# language DeriveFunctor, DerivingVia #-}

-- | This module defines 'Action' and types of that kind.

module Step.ActionTypes.Constructors where

import Step.Internal.Prelude

import Step.RST (RST (..))
import Step.Cursor (CursorRW', CursorR', AdvanceResult)
import Step.Nontrivial

-- | The kind of all the action types in "Step.Action.Types"
type Action =
    Type              -- ^ @xs@ - text
    -> Type           -- ^ @x@ - char
    -> Type           -- ^ @r@ - reader context
    -> Type           -- ^ @s@ - state context
    -> (Type -> Type) -- ^ @m@ - monadic context
    -> Type           -- ^ @a@ - produced upon success
    -> Type

type Any        :: Action
type Atom       :: Action
type AtomicMove :: Action
type Fail       :: Action
type Move       :: Action
type Query      :: Action
type Sure       :: Action
type SureQuery  :: Action


-- | No known properties

data Any xs x r s m a =
    Any_Lift (m a)
  | Any_Ask (r -> a)
  | Any_Get (s -> a)
  | Any_Next (Nontrivial xs x -> a)
  | Any_Commit (Positive Natural) (AdvanceResult -> a)
  | Any_Join (Any xs x r s m (Any xs x r s m a))
  | Any_Fail (r -> r)
  deriving stock Functor

instance Monad m => Applicative (Any xs x r s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Any xs x r s m) where
    return = Any_Lift . return
    a >>= f = Any_Join (fmap f a)


-- | Does not move the cursor

data Query xs x r s m a =
    Query_Lift (m a)
  | Query_Ask (r -> a)
  | Query_Get (s -> a)
  | Query_Next (Nontrivial xs x -> a)
  | Query_Join (Query xs x r s m (Query xs x r s m a))
  | Query_Fail (r -> r)
  deriving stock Functor

instance Monad m => Applicative (Query xs x r s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Query xs x r s m) where
    return = Query_Lift . return
    a >>= f = Query_Join (fmap f a)


-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor

newtype Move xs x r s m a = Move (Any xs x r s m a)
    deriving stock Functor


-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity

newtype Atom xs x r s m a =
    Atom (Query xs x r s m (Sure xs x r s m a))
    deriving stock Functor


-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor

newtype AtomicMove xs x r s m a =
    AtomicMove (Atom xs x r s m a)
    deriving stock Functor


-- | Always succeeds

data Sure xs x r s m a =
    Sure_Lift (m a)
  | Sure_Ask (r -> a)
  | Sure_Get (s -> a)
  | Sure_Next (Nontrivial xs x -> a)
  | Sure_Commit (Positive Natural) (AdvanceResult -> a)
  | Sure_Join (Sure xs x r s m (Sure xs x r s m a))
  deriving stock Functor

instance Monad m => Applicative (Sure xs x r s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Sure xs x r s m) where
    return = Sure_Lift . return
    a >>= f = Sure_Join (fmap f a)


-- | Always succeeds, does not move the cursor

data SureQuery xs x r s m a =
    SureQuery_Lift (m a)
  | SureQuery_Ask (r -> a)
  | SureQuery_Get (s -> a)
  | SureQuery_Next (Nontrivial xs x -> a)
  | SureQuery_Join (SureQuery xs x r s m (SureQuery xs x r s m a))
  deriving stock Functor

instance Monad m => Applicative (SureQuery xs x r s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (SureQuery xs x r s m) where
    return = SureQuery_Lift . return
    a >>= f = SureQuery_Join (fmap f a)


-- | Never succeeds, never moves the cursor, never does anything at all

data Fail xs x r s m a = Fail
    deriving stock Functor
