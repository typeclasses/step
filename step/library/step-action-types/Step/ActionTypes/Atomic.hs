{-# language DataKinds, FlexibleContexts, FunctionalDependencies, InstanceSigs, KindSignatures #-}

module Step.ActionTypes.Atomic where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors
import Step.ActionTypes.Functorial
import Step.ActionTypes.Join
import Step.ActionTypes.Subtyping
import Step.ActionTypes.Fallible

class (FunctorialAction act, FunctorialAction try) =>
    Atomic (act :: Action) (try :: Action) | act -> try
  where
    try :: Monad m => act xs x r s e m a -> try xs x r s e' m (Maybe a)

instance Atomic Atom Sure
  where
    try (Atom x) = castTo @Sure (try @Query x) >>= maybe (return Nothing) (mapError' . fmap Just)

instance Atomic AtomicMove Sure
  where
    try = try @Atom . castTo @Atom

instance Atomic Query SureQuery
  where
    try :: forall xs x r s e e' m a. Monad m =>
        Query xs x r s e m a -> SureQuery xs x r s e' m (Maybe a)
    try = r
      where
        r :: forall a'. Query xs x r s e m a' -> SureQuery xs x r s e' m (Maybe a')
        r = \case
            Query_Base x -> SureQuery (let SureBase y = try x in cast @Base @Query y)
            Query_Join x -> r x >>= maybe (return Nothing) r

instance Atomic Base SureBase
  where
    try = \case
      Base_Fail _ -> SureBase (Base_Lift (return Nothing))
      Base_Lift x -> SureBase (Base_Lift (Just <$> x))
      Base_Ask x -> SureBase (Base_Ask (Just . x))
      Base_Get x -> SureBase (Base_Get (Just . x))
      Base_Reset x -> SureBase (Base_Reset (Just x))
      Base_Next x -> SureBase (Base_Next (Just . x))
