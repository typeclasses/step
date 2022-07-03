module Step.Action.Family where

import Step.Internal.Prelude

import qualified Step.Action.Kind as Kind
import Step.Action.Kind (ActionKind)

data family Action (kind :: ActionKind) :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type

-- | No known properties
--
newtype instance Action 'Kind.Any config cursor error m a =
    Any (config -> cursor -> m (Either error a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT error (StateT cursor m)))

-- | Never moves the cursor
--
newtype instance Action 'Kind.Static config cursor error m a =
    Static (config -> cursor -> m (Either error a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT error (StateT cursor m)))

-- | Always moves the cursor
--
newtype instance Action 'Kind.Move config cursor error m a =
    Move (config -> cursor -> m (Either error a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT error (StateT cursor m)))

-- | Fails noncommittally
--
-- No Applicative/Monad instances here because sequencing does not preserve the noncommittal property
--
newtype instance Action 'Kind.Undo config cursor error m a =
    Undo (config -> cursor -> m (Either error a, cursor))
    deriving stock Functor

-- | Always moves the cursor, fails noncommittally
--
-- No Applicative/Monad instances here because sequencing does not preserve the noncommittal property
--
newtype instance Action 'Kind.MoveUndo config cursor error m a =
    MoveUndo (config -> cursor -> m (Either error a, cursor))
    deriving stock Functor

-- | Always succeeds
--
newtype instance Action 'Kind.Sure config cursor error m a =
    Sure (config -> cursor -> m (a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, never moves the cursor
--
newtype instance Action 'Kind.SureStatic config cursor error m a =
    SureStatic (config -> cursor -> m (a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, always moves the cursor
--
newtype instance Action 'Kind.SureMove config cursor error m a =
    SureMove (config -> cursor -> m (a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

---

class Configurable (kind :: ActionKind) where
    configure :: (config -> config)
        -> Action kind config cursor error m a
        -> Action kind config cursor error m a

instance Configurable 'Kind.Any where configure f (Any g) = Any (g . f)
instance Configurable 'Kind.Static where configure f (Static g) = Static (g . f)
instance Configurable 'Kind.Move where configure f (Move g) = Move (g . f)
instance Configurable 'Kind.Undo where configure f (Undo g) = Undo (g . f)
instance Configurable 'Kind.MoveUndo where configure f (MoveUndo g) = MoveUndo (g . f)
instance Configurable 'Kind.Sure where configure f (Sure g) = Sure (g . f)
instance Configurable 'Kind.SureStatic where configure f (SureStatic g) = SureStatic (g . f)
instance Configurable 'Kind.SureMove where configure f (SureMove g) = SureMove (g . f)

-- data RefinedAction (config :: Type) (cursor :: Type) (error :: Type) (kind :: ActionKind) (m :: Type -> Type) (a :: Type)
--   where
--     RefinedAction'General ::
--         FallibilityOf kind ~ 'MightFail =>
--         (config -> state -> m (Either error a, state))
--         -> RefinedAction config cursor error kind m a
--     RefinedAction'Certain ::
--         FallibilityOf kind ~ 'AlwaysSucceeds =>
--         (config -> state -> m (a, state))
--         -> RefinedAction config cursor error kind m a

-- deriving stock instance Functor m => Functor (RefinedAction config cursor error kind m)

-- instance (Monad m, IsSequenceable (CommitmentOf kind) ~ 'True, Trivial kind) => Applicative (RefinedAction config cursor error kind m)
--   where
--     pure = trivial

-- class Trivial (kind :: ActionKind) where
--     trivial :: Monad m => a -> RefinedAction config cursor error kind m a
-- instance Trivial 'Any where trivial = trivialGeneral
-- instance Trivial 'Static where trivial = trivialGeneral
-- instance Trivial 'Move where trivial = trivialGeneral
-- instance Trivial 'Undo where trivial = trivialGeneral

-- trivialGeneral x =  RefinedAction'General \_ s -> return (Right x, s)

-- instance Monad m => Applicative (RefinedAction config cursor error 'Any m)
-- instance Monad m => Monad (RefinedAction config cursor error 'Any m)

-- Monads: any, static, move, sure, surestatic, suremove
