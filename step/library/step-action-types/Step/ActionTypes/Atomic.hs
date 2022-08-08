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
    try (Atom x) = cast @Sure (try @Query x) >>= maybe (return Nothing) (mapError' . fmap Just)

instance Atomic AtomicMove Sure
  where
    try = try @Atom . cast @Atom

instance Atomic Query SureQuery
  where
    try :: forall xs x r s e e' m a. Monad m =>
        Query xs x r s e m a -> SureQuery xs x r s e' m (Maybe a)
    try = r
      where
        r :: forall a'. Query xs x r s e m a' -> SureQuery xs x r s e' m (Maybe a')
        r = \case
            Query_Lift x -> SureQuery (Query_Lift (Just <$> x))
            Query_Ask f -> SureQuery (Query_Ask (Just . f))
            Query_Get f -> SureQuery (Query_Get (Just . f))
            Query_Next f -> SureQuery (Query_Next (Just . f))
            Query_Join x -> r x >>= maybe (return Nothing) r
            Query_Fail _ -> return Nothing
