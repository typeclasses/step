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
            Any_Join x -> Any_Join (go (fmap go x))
            Any_Base x -> Any_Base (mapError f x)
            Any_Commit n x -> Any_Commit n x

instance Fallible Query where
    mapError :: forall e e' xs x r s m a. Functor m =>
        (e -> e') -> Query xs x r s e m a -> Query xs x r s e' m a
    mapError f = go
      where
        go :: forall a'. Query xs x r s e m a' -> Query xs x r s e' m a'
        go = \case
            Query_Join x -> Query_Join (go (fmap go x))
            Query_Base x -> Query_Base (mapError f x)

instance Fallible Base where
    mapError f = \case
        Base_Fail g -> Base_Fail (f . g)
        Base_Lift x -> Base_Lift x
        Base_Ask x -> Base_Ask x
        Base_Get x -> Base_Get x
        Base_Reset x -> Base_Reset x
        Base_Next x -> Base_Next x

instance Fallible Atom where
    mapError f (Atom q) = Atom (mapError f (fmap mapError' q))

instance Fallible Fail where
    mapError f (Fail x) = Fail \r -> f (x r)

deriving newtype instance Fallible Move

deriving newtype instance Fallible AtomicMove

instance Infallible Sure where
    mapError' (Sure x) = Sure (mapError absurd x)

instance Infallible SureQuery where
    mapError' (SureQuery x) = SureQuery (mapError absurd x)

instance Infallible SureBase where
    mapError' (SureBase x) = SureBase (mapError absurd x)
