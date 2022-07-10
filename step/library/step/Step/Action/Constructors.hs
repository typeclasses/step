{-# language DataKinds, StandaloneKindSignatures, Unsafe #-}

{-# language DeriveFunctor, DerivingVia #-}

{-|

This module defines 'ActionKind' and types of that kind.

== Actions

The actions are named for particular properties they have:

* 'Move' — always advances the cursor if it succeeds
* 'Atom' — either fails or advances the cursor, never both
* 'Sure' — never fails
* 'Query' — never advances the cursor; only gives information about the present state

There are also some that have combinations of the properties described above:

* 'AtomicMove' — atomic, and always advances if it succeeds
* 'SureQuery' — never fails, and never advances

Additionally, we have a type representing an action that always fails:

* 'Fail' — never succeeds and never advances; this vacuously supports /Move/, /Atom/, and /Query/ properties

Finally, there is one with no particular properties:

* 'Any' — the most general type of action; all others can be lifted to it


=== Actions not defined

Several conceivable combinations of properties are not defined as action types:

* /Sure/ + /Move/ — because there's no such thing as a sure move; since at the end of input there is nowhere to move
* /Atom/ + /Query/ — because this is just 'Query'; queries never move the cursor, so they are necessarily atomic
* /Atom/ + /Sure/ — because this is just 'Sure'; a sure action cannot fail, so it necessary cannot fail and move the cursor
* /Move/ + /Query/ — this would be a contradiction, if the action ever succeeded; 'Move' means always advance and 'Query' means never advance
* A non-atomic failure type, which is allowed to move the cursor but always fails, is not defined because it is of no use


== Unsafety

The /Sure/ property is guaranteed by construction. The rest of the properties are not. This module is, therefore, unsafe. See "Step.Action.Types" and "Step.Action.Safe".

-}
module Step.Action.Constructors where

import Step.Internal.Prelude

-- | The kind of all the action types in "Step.Action.Types"
type ActionKind =
       Type           -- ^ @config@ - read-only
    -> Type           -- ^ @cursor@ - mutable state
    -> Type           -- ^ @error@  - produced upon failure
    -> (Type -> Type) -- ^ @base@   - monadic context
    -> Type           -- ^ @value@  - produced upon success
    -> Type

type Any        :: ActionKind
type Query      :: ActionKind
type Move       :: ActionKind
type Atom       :: ActionKind
type AtomicMove :: ActionKind
type Sure       :: ActionKind
type SureQuery  :: ActionKind

-- | No known properties
--
newtype Any config cursor error base value =
    Any (config -> StateT cursor base (Either (StateT cursor base error) value))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT (StateT cursor base error) (StateT cursor base)))

-- | Does not move the cursor
--
newtype Query config cursor error base value =
    Query (config -> StateT cursor base (Either (StateT cursor base error) value))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT (StateT cursor base error) (StateT cursor base)))

-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor
--
newtype Move config cursor error base value =
    Move (config -> StateT cursor base (Either (StateT cursor base error) value))
    deriving Functor
        via (ReaderT config (ExceptT (StateT cursor base error) (StateT cursor base)))

-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity
--
newtype Atom config cursor error base value =
    Atom (config -> StateT cursor base (Either (StateT cursor base error) value))
    deriving stock Functor

-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor
--
newtype AtomicMove config cursor error base value =
    AtomicMove (config -> StateT cursor base (Either (StateT cursor base error) value))
    deriving stock Functor

-- | Always succeeds
--
newtype Sure config cursor error base value =
    Sure (config -> StateT cursor base value)
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor base))

-- | Always succeeds, does not move the cursor
--
newtype SureQuery config cursor error base value =
    SureQuery (config -> StateT cursor base value)
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor base))

-- | Never succeeds and never moves the cursor, which vacuously guarantees 'Move.
--
newtype Fail config cursor error base value =
    Fail (config -> StateT cursor base error)
