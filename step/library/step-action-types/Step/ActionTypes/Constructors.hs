{-# language DataKinds, StandaloneKindSignatures, Unsafe #-}

{-# language DeriveFunctor, DerivingVia #-}

-- | This module defines 'ActionKind' and types of that kind.

module Step.ActionTypes.Constructors where

import Step.Internal.Prelude

import Step.RST (RST (..))
import Step.Cursor (ReadWriteCursor (..))

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

newtype Any xs x r s m a = Any (ReadWriteCursor xs x r s m -> RST r s m (Maybe a))
    deriving (Functor, Applicative, Monad) via (ReaderT (ReadWriteCursor xs x r s m) (MaybeT (RST r s m)))

-- | Does not move the cursor

newtype Query xs x r s m a = Query (ReadWriteCursor xs x r s m -> RST r s m (Maybe a))
    deriving (Functor, Applicative, Monad) via (ReaderT (ReadWriteCursor xs x r s m) (MaybeT (RST r s m)))

-- | Always moves the cursor
--
-- No 'Applicative' or 'Monad' instance here because 'pure' and 'return' don't move the cursor

newtype Move xs x r s m a = Move (ReadWriteCursor xs x r s m -> RST r s m (Maybe a))
    deriving stock Functor

-- | Fails noncommittally
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity

newtype Atom xs x r s m a =
    Atom (ReadWriteCursor xs x r s m -> RST r s m (Maybe a))
    deriving stock Functor

-- | Always moves the cursor, is atomic
--
-- No 'Applicative' or 'Monad' instance here because sequencing does not preserve atomicity, and because 'pure' and 'return' don't move the cursor

newtype AtomicMove xs x r s m a =
    AtomicMove (ReadWriteCursor xs x r s m -> RST r s m (Maybe a))
    deriving stock Functor

-- | Always succeeds

newtype Sure xs x r s m a =
    Sure (ReadWriteCursor xs x r s m -> RST r s m a)
    deriving (Functor, Applicative, Monad) via (ReaderT (ReadWriteCursor xs x r s m) (RST r s m))

-- | Always succeeds, does not move the cursor

newtype SureQuery xs x r s m a =
    SureQuery (ReadWriteCursor xs x r s m -> RST r s m a)
    deriving (Functor, Applicative, Monad) via (ReaderT (ReadWriteCursor xs x r s m) (RST r s m))

-- | Never succeeds and never moves the cursor

newtype Fail xs x r s m a = Fail (ReadWriteCursor xs x r s m -> RST r s m ())
    deriving stock Functor
