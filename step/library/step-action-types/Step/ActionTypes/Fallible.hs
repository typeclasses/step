{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Step.ActionTypes.Fallible
  (
    Fallible (..),
    Infallible (..),
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class Fallible (act :: Action) where
    mapError :: Functor m => (e -> e') -> act xs x r s e m a -> act xs x r s e' m a

class Infallible (act :: Action) where
    mapError' :: Functor m => act xs x r s e m a -> act xs x r s e' m a

instance Fallible Any where
    mapError :: forall e e' xs x r s m a. Functor m =>
        (e -> e') -> Any xs x r s e m a -> Any xs x r s e' m a
    mapError f = go
      where
        go :: forall a'. Any xs x r s e m a' -> Any xs x r s e' m a'
        go = \case
            Any_Fail g -> Any_Fail \r s -> f (g r s)
            Any_Join x -> Any_Join (go (fmap go x))
            Any_Lift x -> Any_Lift x
            Any_Ask x -> Any_Ask x
            Any_Get x -> Any_Get x
            Any_Next x -> Any_Next x
            Any_Commit n x -> Any_Commit n x

instance Fallible Query where
    mapError :: forall e e' xs x r s m a. Functor m =>
        (e -> e') -> Query xs x r s e m a -> Query xs x r s e' m a
    mapError f = go
      where
        go :: forall a'. Query xs x r s e m a' -> Query xs x r s e' m a'
        go = \case
            Query_Fail g -> Query_Fail \r s -> f (g r s)
            Query_Join x -> Query_Join (go (fmap go x))
            Query_Lift x -> Query_Lift x
            Query_Ask x -> Query_Ask x
            Query_Get x -> Query_Get x
            Query_Next x -> Query_Next x

instance Fallible Atom where
    mapError f (Atom q) = Atom (mapError f (fmap mapError' q))

instance Fallible Fail where
    mapError f (Fail x) = Fail \r s -> f (x r s)

deriving newtype instance Fallible Move

deriving newtype instance Fallible AtomicMove

instance Infallible Sure where
    mapError' (Sure x) = Sure (mapError absurd x)

instance Infallible SureQuery where
    mapError' (SureQuery x) = SureQuery (mapError absurd x)
