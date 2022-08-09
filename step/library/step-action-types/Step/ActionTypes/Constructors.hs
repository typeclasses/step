{-# language DataKinds, StandaloneKindSignatures #-}

{-# language DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving #-}

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

type Any        :: Action
type Atom       :: Action
type AtomicMove :: Action
type Fail       :: Action
type Move       :: Action
type Query      :: Action
type Sure       :: Action
type SureQuery  :: Action


-- | No known properties

data Any xs x r s e m a =
    Any_Lift (m a)
  | Any_Ask (r -> a)
  | Any_Get (s -> a)
  | Any_Next (Maybe (Nontrivial xs x) -> a)
  | Any_Commit (Positive Natural) a
  | Any_Join (Any xs x r s e m (Any xs x r s e m a))
  | Any_Fail (r -> e)
  | Any_Reset a
  deriving stock Functor

instance Monad m => Applicative (Any xs x r s e m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Any xs x r s e m) where
    return = Any_Lift . return
    a >>= f = Any_Join (fmap f a)


-- | Does not move the cursor

data Query xs x r s e m a =
    Query_Lift (m a)
  | Query_Ask (r -> a)
  | Query_Get (s -> a)
  | Query_Next (Maybe (Nontrivial xs x) -> a)
  | Query_Join (Query xs x r s e m (Query xs x r s e m a))
  | Query_Fail (r -> e)
  | Query_Reset a
  deriving stock Functor

instance Monad m => Applicative (Query xs x r s e m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Query xs x r s e m) where
    return = Query_Lift . return
    a >>= f = Query_Join (fmap f a)


-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor

newtype Move xs x r s e m a = Move (Any xs x r s e m a)
    deriving stock Functor


-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity

newtype Atom xs x r s e m a =
    Atom (Query xs x r s e m (Sure xs x r s e m a))
    deriving stock Functor


-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor

newtype AtomicMove xs x r s e m a =
    AtomicMove (Atom xs x r s e m a)
    deriving stock Functor


-- | Always succeeds

newtype Sure xs x r s e m a =
    Sure (Any xs x r s Void m a)
    deriving newtype (Functor, Applicative, Monad)


-- | Always succeeds, does not move the cursor

newtype SureQuery xs x r s e m a =
    SureQuery (Query xs x r s Void m a)
    deriving newtype (Functor, Applicative, Monad)


-- | Never succeeds, never moves the cursor, never does anything at all

data Fail xs x r s e m a = Fail (r -> e)
    deriving stock Functor
