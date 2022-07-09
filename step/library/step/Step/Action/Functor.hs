{-# language DataKinds, KindSignatures, QuantifiedConstraints, Safe #-}

module Step.Action.Functor where

import Step.Internal.Prelude

import Step.Action.Types

import Step.Action.IsAction

class (IsAction action, forall config cursor error m. Monad m =>
      Monad (action config cursor error m)) => MonadAction (action :: ActionKind)

instance MonadAction Any
instance MonadAction Query
instance MonadAction Sure
instance MonadAction SureQuery
