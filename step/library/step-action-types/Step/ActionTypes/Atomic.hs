{-# language DataKinds, FlexibleContexts, FunctionalDependencies, InstanceSigs, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Atomic where

import Step.Internal.Prelude

import Step.ActionTypes.Types
import Step.ActionTypes.Constructors
import Step.ActionTypes.Functorial
import Step.ActionTypes.Join
import Step.ActionTypes.Subtyping

class (FunctorialAction act, FunctorialAction try) =>
    Atomic (act :: Action) (try :: Action) | act -> try
  where
    try :: Monad m => act xs x r s m a -> try xs x r s m (Maybe a)

instance Atomic Atom Sure
  where
    try (Atom x) = cast @Sure (try @Query x) >>= maybe (return Nothing) (fmap Just)

instance Atomic AtomicMove Sure
  where
    try = try @Atom . cast @Atom

instance Atomic Query SureQuery
  where
    try :: forall xs x r s m a. Monad m =>
        Query xs x r s m a -> SureQuery xs x r s m (Maybe a)
    try = r
      where
        r :: forall a'. Query xs x r s m a' -> SureQuery xs x r s m (Maybe a')
        r = \case
            Query_Lift x -> Just <$> SureQuery_Lift x
            Query_Ask f -> Just <$> SureQuery_Ask f
            Query_Get f -> Just <$> SureQuery_Get f
            Query_Next f -> Just <$> SureQuery_Next f
            Query_Join x -> r x >>= maybe (return Nothing) r
            Query_Fail _ -> return Nothing
