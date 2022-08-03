{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, Safe, TypeOperators #-}

module Step.ActionTypes.Functorial where

import Step.Internal.Prelude

import Step.ActionTypes.Types

class (forall xs x r s m. Functor m => Functor (act xs x r s m)) => FunctorialAction (act :: Action)

instance FunctorialAction Any
instance FunctorialAction Atom
instance FunctorialAction AtomicMove
instance FunctorialAction Fail
instance FunctorialAction Move
instance FunctorialAction Query
instance FunctorialAction Sure
instance FunctorialAction SureQuery
