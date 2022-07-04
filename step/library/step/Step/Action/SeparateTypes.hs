module Step.Action.SeparateTypes where

import Step.Internal.Prelude

import Variado.Monad.Class

import qualified Monad

type ActionKind = Type -> Type -> Type -> (Type -> Type) -> Type -> Type

-- type Any        :: ActionKind
-- type Static     :: ActionKind
-- type Move       :: ActionKind
-- type Undo       :: ActionKind
-- type MoveUndo   :: ActionKind
-- type Sure       :: ActionKind
-- type SureStatic :: ActionKind
-- type SureMove   :: ActionKind

-- | No known properties
--
newtype Any config cursor error m a =
    Any (config -> cursor -> m (Either error a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT error (StateT cursor m)))

-- | Does not move the cursor
--
newtype Static config cursor error m a =
    Static (config -> cursor -> m (Either error a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT error (StateT cursor m)))

-- | Always moves the cursor
--
newtype Move config cursor error m a =
    Move (config -> cursor -> m (Either error a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (ExceptT error (StateT cursor m)))

-- | Fails noncommittally
--
-- No Applicative/Monad instances here because sequencing does not preserve the noncommittal property
--
newtype Undo config cursor error m a =
    Undo (config -> cursor -> m (Either error a, cursor))
    deriving stock Functor

-- | Always moves the cursor, fails noncommittally
--
-- No Applicative/Monad instances here because sequencing does not preserve the noncommittal property
--
newtype MoveUndo config cursor error m a =
    MoveUndo (config -> cursor -> m (Either error a, cursor))
    deriving stock Functor

-- | Always succeeds
--
newtype Sure config cursor error m a =
    Sure (config -> cursor -> m (a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, does not move the cursor
--
newtype SureStatic config cursor error m a =
    SureStatic (config -> cursor -> m (a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

-- | Always succeeds, always moves the cursor
--
newtype SureMove config cursor error m a =
    SureMove (config -> cursor -> m (a, cursor))
    deriving (Functor, Applicative, Monad)
        via (ReaderT config (StateT cursor m))

---

sureToAny :: Functor m => Sure config cursor error m a -> Any config cursor error m a
sureToAny (Sure p) = Any (\c s -> p c s <&> \(x, s') -> (Right x, s'))

---

joinAnyToSure :: Monad m => Any config cursor error m (Sure config cursor error m a) -> Any config cursor error m a
joinAnyToSure (Any p) =
    Any \c s -> do
        (e, s') <- p c s
        case e of
            Left e' -> return (Left e', s')
            Right (Sure p') -> do
                (x, s'') <- p' c s'
                return (Right x, s'')

joinSureToAny :: Monad m => Sure config cursor error m (Any config cursor error m a) -> Any config cursor error m a
joinSureToAny (Sure p) =
    Any \c s -> do
        (Any p', s') <- p c s
        p' c s'

---

class Like k k'
  where
    actionIso :: Iso
        (k' config1 cursor1 error1 m1 a1)
        (k' config2 cursor2 error2 m2 a2)
        (k  config1 cursor1 error1 m1 a1)
        (k  config2 cursor2 error2 m2 a2)

actionIso' :: forall k k' config1 cursor1 error1 m1 a1 config2 cursor2 error2 m2 a2.
    Like k k' => Iso
        (k' config1 cursor1 error1 m1 a1)
        (k' config2 cursor2 error2 m2 a2)
        (k  config1 cursor1 error1 m1 a1)
        (k  config2 cursor2 error2 m2 a2)
actionIso' = actionIso

instance Like Any Any         where actionIso = coerced
instance Like Any Static      where actionIso = coerced
instance Like Any Move        where actionIso = coerced
instance Like Any Undo        where actionIso = coerced
instance Like Any MoveUndo    where actionIso = coerced

instance Like Sure Sure       where actionIso = coerced
instance Like Sure SureStatic where actionIso = coerced
instance Like Sure SureMove   where actionIso = coerced

---

class FunctorAction (action :: ActionKind) where
    fmapAction :: Functor m => (a -> b)
        -> action config cursor error m a
        -> action config cursor error m b

instance FunctorAction Any        where fmapAction = fmap
instance FunctorAction Static     where fmapAction = fmap
instance FunctorAction Move       where fmapAction = fmap
instance FunctorAction Undo       where fmapAction = fmap
instance FunctorAction MoveUndo   where fmapAction = fmap
instance FunctorAction Sure       where fmapAction = fmap
instance FunctorAction SureStatic where fmapAction = fmap
instance FunctorAction SureMove   where fmapAction = fmap

---

class MonadAction (action :: ActionKind) where
    pureAction :: Monad m => a -> action config cursor error m a
    bindAction :: Monad m => action config cursor error m a -> (a -> action config cursor error m b) -> action config cursor error m b

instance MonadAction Any        where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Static     where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Move       where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Sure       where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction SureStatic where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction SureMove   where pureAction = pure ; bindAction = (Monad.>>=)

---

class ConfigurableAction (action :: ActionKind) where
    configureAction :: (config1 -> config2)
        -> action config2 cursor error m a
        -> action config1 cursor error m a

instance ConfigurableAction Any where
    configureAction f (Any g) = Any (g . f)
instance ConfigurableAction Static where
    configureAction f (Static g) = Static (g . f)
instance ConfigurableAction Move where
    configureAction f (Move g) = Move (g . f)
instance ConfigurableAction Undo where
    configureAction f (Undo g) = Undo (g . f)
instance ConfigurableAction MoveUndo where
    configureAction f (MoveUndo g) = MoveUndo (g . f)
instance ConfigurableAction Sure where
    configureAction f (Sure g) = Sure (g . f)
instance ConfigurableAction SureStatic where
    configureAction f (SureStatic g) = SureStatic (g . f)
instance ConfigurableAction SureMove where
    configureAction f (SureMove g) = SureMove (g . f)

-- class Configurable config m | m -> config where
--     configure :: (config -> config) -> m a -> m a

-- instance ConfigurableAction kind => Configurable config (Action kind config cursor error m)
--   where
--     configure f (Action a) = Action (configureAction f a)

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

-- instance
--     forall (k1 :: ActionKind) (k2 :: ActionKind) config cursor error m.
--     (Functor (Action k1 config cursor error m), Functor (Action k2 config cursor error m)) =>
--     PolyMonad (Action k1 config cursor error m) (Action k2 config cursor error m)
--   where
--     type Join (Action k1 config cursor error m) (Action k2 config cursor error m) = (Action (k1 :> k2) config cursor error m)
