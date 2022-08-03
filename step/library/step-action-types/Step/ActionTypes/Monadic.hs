{-# language DataKinds, KindSignatures, QuantifiedConstraints, Safe #-}

module Step.ActionTypes.Monadic where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import Step.ActionTypes.Functorial

import Step.ActionTypes.Returnable

class (FunctorialAction act, Returnable act, forall xs x r s m. Monad m => Monad (act xs x r s m)) =>
    MonadicAction (act :: Action)

instance MonadicAction Any
instance MonadicAction Query
instance MonadicAction Sure
instance MonadicAction SureQuery
