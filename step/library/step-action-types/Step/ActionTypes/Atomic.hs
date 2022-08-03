{-# language DataKinds, FlexibleContexts, FunctionalDependencies, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Atomic where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import Step.ActionTypes.Constructors (Any (..), Sure (..))

import qualified Step.ActionTypes.Coerce as Coerce

import Step.ActionTypes.Functorial

class (FunctorialAction act, FunctorialAction try) =>
    Atomic (act :: Action) (try :: Action) | act -> try
  where
    try :: Functor m => act xs x r s m a -> try xs x r s m (Maybe a)

instance Atomic Atom Sure
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic AtomicMove Sure
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic Query SureQuery
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

tryAnySure :: Functor m => Any xs x r s m a -> Sure xs x r s m (Maybe a)
tryAnySure (Any p) = Sure \c -> p c <&> \case Left _ -> Nothing; Right x -> Just x
