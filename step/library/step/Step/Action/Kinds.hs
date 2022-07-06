module Step.Action.Kinds where

import Step.Internal.Prelude

type ActionKind = Type -> Type -> Type -> (Type -> Type) -> Type -> Type

type Any        :: ActionKind
type Static     :: ActionKind
type Move       :: ActionKind
type Undo       :: ActionKind
type MoveUndo   :: ActionKind
type Sure       :: ActionKind
type SureStatic :: ActionKind
type SureMove   :: ActionKind

-- | No known properties
--
newtype Any config cursor error m a =
    Any (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT (StateT cursor m error) (StateT cursor m)))

-- | Does not move the cursor
--
newtype Static config cursor error m a =
    Static (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT (StateT cursor m error) (StateT cursor m)))

-- | Always moves the cursor
--
-- No Applicative/Monad instances here because pure/return doesn't move the cursor
--
newtype Move config cursor error m a =
    Move (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving Functor
        via (ReaderT config (ExceptT (StateT cursor m error) (StateT cursor m)))

-- | Fails noncommittally
--
-- No Applicative/Monad instances here because sequencing does not preserve the noncommittal property
--
newtype Undo config cursor error m a =
    Undo (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving stock Functor

-- | Always moves the cursor, fails noncommittally
--
-- No Applicative/Monad instances here because sequencing does not preserve the noncommittal property, and because pure/return doesn't move the cursor
--
newtype MoveUndo config cursor error m a =
    MoveUndo (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving stock Functor

-- | Always succeeds
--
newtype Sure config cursor error m a =
    Sure (config -> StateT cursor m a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, does not move the cursor
--
newtype SureStatic config cursor error m a =
    SureStatic (config -> StateT cursor m a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, always moves the cursor
--
-- No Applicative/Monad instances here because pure/return doesn't move the cursor
--
newtype SureMove config cursor error m a =
    SureMove (config -> StateT cursor m a)
    deriving (Functor)
        via (ReaderT config (StateT cursor m))
