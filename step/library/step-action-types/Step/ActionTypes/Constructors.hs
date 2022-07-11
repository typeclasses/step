{-# language DataKinds, StandaloneKindSignatures, Unsafe #-}

{-# language DeriveFunctor, DerivingVia #-}

-- | This module defines 'ActionKind' and types of that kind.

module Step.ActionTypes.Constructors where

import Step.Internal.Prelude

-- | The kind of all the action types in "Step.Action.Types"
type ActionKind =
       Type           -- ^ @cursor@ - mutable state
    -> Type           -- ^ @error@  - produced upon failure
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
newtype Any cursor error base value =
    Any (StateT cursor base (Either (StateT cursor base error) value))
    deriving (Functor, Applicative, Monad)
        via (ExceptT (StateT cursor base error) (StateT cursor base))

-- | Does not move the cursor
--
newtype Query cursor error base value =
    Query (StateT cursor base (Either (StateT cursor base error) value))
    deriving (Functor, Applicative, Monad)
        via (ExceptT (StateT cursor base error) (StateT cursor base))

-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor
--
newtype Move cursor error base value =
    Move (StateT cursor base (Either (StateT cursor base error) value))
    deriving Functor
        via (ExceptT (StateT cursor base error) (StateT cursor base))

-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity
--
newtype Atom cursor error base value =
    Atom (StateT cursor base (Either (StateT cursor base error) value))
    deriving stock Functor

-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor
--
newtype AtomicMove cursor error base value =
    AtomicMove (StateT cursor base (Either (StateT cursor base error) value))
    deriving stock Functor

-- | Always succeeds
--
newtype Sure cursor error base value =
    Sure (StateT cursor base value)
    deriving (Functor, Applicative, Monad)
        via (StateT cursor base)

-- | Always succeeds, does not move the cursor
--
newtype SureQuery cursor error base value =
    SureQuery (StateT cursor base value)
    deriving (Functor, Applicative, Monad)
        via (StateT cursor base)

-- | Never succeeds and never moves the cursor
--
newtype Fail cursor error base value =
    Fail (StateT cursor base error)
    deriving stock Functor
