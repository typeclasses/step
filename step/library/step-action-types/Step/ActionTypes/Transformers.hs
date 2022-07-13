{-# language DataKinds, FlexibleContexts, KindSignatures, Trustworthy, TypeOperators #-}
{-# language DerivingVia, StandaloneDeriving #-}

module Step.ActionTypes.Transformers where

import Step.Internal.Prelude

import Step.ActionTypes.Types
import Step.ActionTypes.KindJoin
import Step.ActionTypes.Join
import Step.ActionTypes.Functorial
import Step.ActionTypes.Monadic
import Step.ActionTypes.Subtyping

---

newtype ActionIdentity error (base :: Type -> Type) (kind :: ActionKind) value =
    ActionIdentity{ runActionIdentity :: kind error base value }

deriving
    via ((kind :: ActionKind) error (base :: Type -> Type))
    instance (Functor base, FunctorialAction kind) =>
        Functor (ActionIdentity error base kind)

deriving
    via ((kind :: ActionKind) error (base :: Type -> Type))
    instance (Monad base, MonadicAction kind) =>
        Applicative (ActionIdentity error base kind)

deriving
    via ((kind :: ActionKind) error (base :: Type -> Type))
    instance (Monad base, MonadicAction kind) =>
        Monad (ActionIdentity error base kind)

---

newtype ActionReader context error (base :: Type -> Type) (kind :: ActionKind) value =
    ActionReader{ runActionReader :: context -> kind error base value }

deriving
    via (ReaderT context ((kind :: ActionKind) error (base :: Type -> Type)))
    instance (Functor base, FunctorialAction kind) =>
        Functor (ActionReader context error base kind)

deriving
    via (ReaderT context ((kind :: ActionKind) error (base :: Type -> Type)))
    instance (Monad base, MonadicAction kind) =>
        Applicative (ActionReader context error base kind)

deriving
    via (ReaderT context ((kind :: ActionKind) error (base :: Type -> Type)))
    instance (Monad base, MonadicAction kind) =>
        Monad (ActionReader context error base kind)

---

class ActionT (m :: ActionKind -> Type -> Type)
  where
    joinT :: Join k1 k2 =>
        (FunctorialAction k1, FunctorialAction k2, FunctorialAction (k1 >> k2)) =>
        m k1 (m k2 a) -> m (k1 >> k2) a
    castT :: Is k1 k2 =>
        m k1 a -> m k2 a

instance Monad base => ActionT (ActionIdentity error base)
  where
    joinT = ActionIdentity . join . fmap runActionIdentity . runActionIdentity
    castT = ActionIdentity . cast . runActionIdentity

instance Monad base => ActionT (ActionReader context error base)
  where
    joinT a = ActionReader \c -> join $ fmap (\b -> runActionReader b c) $ runActionReader a c
    castT a = ActionReader \c -> cast $ runActionReader a c
