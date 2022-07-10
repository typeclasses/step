{-# language DataKinds, KindSignatures, QuantifiedConstraints, Safe #-}

module Step.ActionTypes.Monadic where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import Step.ActionTypes.Functorial

import Step.ActionTypes.Returnable

class (FunctorialAction action, Returnable action, forall config cursor error m. Monad m =>
      Monad (action config cursor error m)) => MonadicAction (action :: ActionKind)

instance MonadicAction Any
instance MonadicAction Query
instance MonadicAction Sure
instance MonadicAction SureQuery
