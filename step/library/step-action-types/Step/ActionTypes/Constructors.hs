{-# language DataKinds, StandaloneKindSignatures, Unsafe #-}

{-# language DeriveFunctor, DerivingVia #-}

-- | This module defines 'ActionKind' and types of that kind.

module Step.ActionTypes.Constructors where

import Step.Internal.Prelude

-- | The kind of all the action types in "Step.Action.Types"
type ActionKind =
       Type           -- ^ @error@  - produced upon failure
    -> (Type -> Type) -- ^ @base@   - monadic context
    -> Type           -- ^ @value@  - produced upon success
    -> Type

type Any        :: ActionKind
type Atom       :: ActionKind
type AtomicMove :: ActionKind
type Fail       :: ActionKind
type Move       :: ActionKind
type Query      :: ActionKind
type Sure       :: ActionKind
type SureQuery  :: ActionKind

-- | No known properties
--
newtype Any error base value =
    Any (base (Either (base error) value))
    deriving (Functor, Applicative, Monad)
        via (ExceptT (base error) base)

-- | Does not move the cursor
--
newtype Query error base value =
    Query (base (Either (base error) value))
    deriving (Functor, Applicative, Monad)
        via (ExceptT (base error) base)

-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor
--
newtype Move error base value =
    Move (base (Either (base error) value))
    deriving stock Functor

-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity
--
newtype Atom error base value =
    Atom (base (Either (base error) value))
    deriving stock Functor

-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor
--
newtype AtomicMove error base value =
    AtomicMove (base (Either (base error) value))
    deriving stock Functor

-- | Always succeeds
--
newtype Sure error base value =
    Sure (base value)
    deriving (Functor, Applicative, Monad)
        via base

-- | Always succeeds, does not move the cursor
--
newtype SureQuery error base value =
    SureQuery (base value)
    deriving (Functor, Applicative, Monad)
        via base

-- | Never succeeds and never moves the cursor
--
newtype Fail error base value =
    Fail (base error)
    deriving stock Functor
