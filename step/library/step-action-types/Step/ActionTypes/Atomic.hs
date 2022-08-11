{-# language DataKinds, FlexibleContexts, FunctionalDependencies, InstanceSigs, KindSignatures #-}

module Step.ActionTypes.Atomic where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors
import Step.ActionTypes.Join

class (FunctorialAction act, FunctorialAction try) =>
    Atomic (act :: Action) (try :: Action) | act -> try
  where
    try :: Functor m => act xs x r s e m a -> try xs x r s e m (Maybe a)

instance Atomic Atom Sure
  where
    try (Atom x) = castTo @Sure (try @Query x) >>= maybe (return Nothing) (fmap Just)

instance Atomic AtomicMove Sure
  where
    try = try @Atom . castTo @Atom

instance Atomic Query SureQuery
  where
    try :: forall xs x r s e m a. Functor m => Query xs x r s e m a -> SureQuery xs x r s e m (Maybe a)
    try (Query (FreeAction q)) = SureQuery (FreeAction (freeTry q))

freeTry :: Functor m => Free (Base xs x r s e m) a -> Free (SureBase xs x r s e m) (Maybe a)
freeTry = \case
    Pure x -> return (Just x)
    Free b -> case b of
        Base_Fail _ -> return Nothing
        Base_RST x -> liftF (SureBase (Base_RST x)) >>= freeTry
        Base_Reset x -> liftF (SureBase (Base_Reset x)) >>= freeTry
        Base_Next x -> liftF (SureBase (Base_Next x)) >>= freeTry
