module Step.Action.Kinds where

import Step.Internal.Prelude

type ActionKind = Type -> Type -> Type -> (Type -> Type) -> Type -> Type

type Any       :: ActionKind
type Query     :: ActionKind
type Move      :: ActionKind
type Atom      :: ActionKind
type MoveAtom  :: ActionKind
type Sure      :: ActionKind
type SureQuery :: ActionKind
type SureMove  :: ActionKind

-- | No known properties
--
newtype Any config cursor error m a =
    Any (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT (StateT cursor m error) (StateT cursor m)))

-- | Does not move the cursor
--
newtype Query config cursor error m a =
    Query (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT (StateT cursor m error) (StateT cursor m)))

-- | Always moves the cursor
--
-- No Applicative or Monad instance here because pure and return don't move the cursor
--
newtype Move config cursor error m a =
    Move (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving Functor
        via (ReaderT config (ExceptT (StateT cursor m error) (StateT cursor m)))

-- | Fails noncommittally
--
-- No Applicative or Monad instance here because sequencing does not preserve the noncommittal property
--
newtype Atom config cursor error m a =
    Atom (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving stock Functor

-- | Always moves the cursor, fails noncommittally
--
-- No Applicative or Monad instance here because sequencing does not preserve the noncommittal property, and because pure and return don't move the cursor
--
newtype MoveAtom config cursor error m a =
    MoveAtom (config -> StateT cursor m (Either (StateT cursor m error) a))
    deriving stock Functor

-- | Always succeeds
--
newtype Sure config cursor error m a =
    Sure (config -> StateT cursor m a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, does not move the cursor
--
newtype SureQuery config cursor error m a =
    SureQuery (config -> StateT cursor m a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, always moves the cursor
--
-- No Applicative or Monad instance here because pure and return don't move the cursor
--
newtype SureMove config cursor error m a =
    SureMove (config -> StateT cursor m a)
    deriving (Functor)
        via (ReaderT config (StateT cursor m))
