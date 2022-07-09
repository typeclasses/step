{-# language DataKinds, KindSignatures, QuantifiedConstraints, Safe #-}

module Step.Action.Monadic where

import Step.Internal.Prelude

import Step.Action.Types

import Step.Action.IsAction

class (IsAction action, forall config cursor error m. Monad m =>
      Monad (action config cursor error m)) => MonadicAction (action :: ActionKind)

instance MonadicAction Any
instance MonadicAction Query
instance MonadicAction Sure
instance MonadicAction SureQuery
