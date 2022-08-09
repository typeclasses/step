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
type Base       :: Action
type Fail       :: Action
type Move       :: Action
type Query      :: Action
type Sure       :: Action
type SureBase   :: Action
type SureQuery  :: Action


data Base xs x r s e m a =
    Base_Lift (m a)
  | Base_Ask (r -> a)
  | Base_Get (s -> a)
  | Base_Reset a
  | Base_Next (Maybe (Nontrivial xs x) -> a)
  | Base_Fail (r -> e)
  deriving stock Functor

newtype SureBase xs x r s e m a =
    SureBase (Base xs x r s Void m a)
    deriving stock Functor


-- | No known properties

data Any xs x r s e m a =
    Any_Base (Base xs x r s e m a)
  | Any_Commit (Positive Natural) a
  | Any_Join (Any xs x r s e m (Any xs x r s e m a))
  deriving stock Functor

instance Monad m => Applicative (Any xs x r s e m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Any xs x r s e m) where
    return = Any_Base . Base_Lift . return
    a >>= f = Any_Join (fmap f a)


-- | Does not move the cursor

data Query xs x r s e m a =
    Query_Base (Base xs x r s e m a)
  | Query_Join (Query xs x r s e m (Query xs x r s e m a))
  deriving stock Functor

instance Monad m => Applicative (Query xs x r s e m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Query xs x r s e m) where
    return = Query_Base . Base_Lift . return
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
