{-# language DataKinds, StandaloneKindSignatures, Unsafe #-}

{-# language DeriveFunctor, DerivingVia #-}

-- | This module defines 'ActionKind' and types of that kind.

module Step.ActionTypes.Constructors where

import Step.Internal.Prelude

-- | The kind of all the action types in "Step.Action.Types"
type Action =
    (Type -> Type)    -- ^ @m@ - monadic context
    -> Type           -- ^ @e@ - produced upon failure
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

newtype Any m e a = Any (m (Either (m e) a))
    deriving (Functor, Applicative, Monad) via (ExceptT (m e) m)

-- | Does not move the cursor

newtype Query m e a = Query (m (Either (m e) a))
    deriving (Functor, Applicative, Monad) via (ExceptT (m e) m)

-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor

newtype Move m e a = Move (m (Either (m e) a))
    deriving stock Functor

-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity

newtype Atom m e a =
    Atom (m (Either (m e) a))
    deriving stock Functor

-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor

newtype AtomicMove m e a = AtomicMove (m (Either (m e) a))
    deriving stock Functor

-- | Always succeeds

newtype Sure m e a = Sure (m a)
    deriving (Functor, Applicative, Monad) via m

-- | Always succeeds, does not move the cursor

newtype SureQuery m e a = SureQuery (m a)
    deriving (Functor, Applicative, Monad) via m

-- | Never succeeds and never moves the cursor

newtype Fail m e a = Fail (m e)
    deriving stock Functor
