{-# language DataKinds, StandaloneKindSignatures, Unsafe #-}

{-# language DeriveFunctor, DerivingVia #-}

{-|

This module defines 'ActionKind' and types of that kind.

+--------------+----------+------------+------------+
|              | Succeeds | Advances   | Advances   |
|              |          | on success | on failure |
+--------------+----------+------------+------------+
| 'Move'       |          | Yes        |            |
+--------------+----------+------------+------------+
| 'Query'      |          | No         | No         |
+--------------+----------+------------+------------+
| 'Atom'       |          |            | No         |
+--------------+----------+------------+------------+
| 'AtomicMove' |          | Yes        | No         |
+--------------+----------+------------+------------+
| 'Sure'       | Yes      |            |            |
+--------------+----------+------------+------------+
| 'SureQuery'  | Yes      | No         | No         |
+--------------+----------+------------+------------+
| 'Fail'       | No       | No         | No         |
+--------------+----------+------------+------------+
| 'Any'        |          |            |            |
+--------------+----------+------------+------------+

The only properties guaranteed by construction are that /Sure/ always succeeds and /Fail/ never does. The rest of the properties are not enforced by constructors. This module is, therefore, unsafe. See "Step.Action.Types" and "Step.Action.Safe".

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

-- | Never succeeds and never moves the cursor
--
newtype Fail config cursor error base value =
    Fail (config -> StateT cursor base error)
    deriving stock Functor
