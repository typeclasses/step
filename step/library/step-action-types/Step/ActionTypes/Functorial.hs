{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}

module Step.ActionTypes.Functorial where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class (forall xs x r s e m. Functor m => Functor (act xs x r s e m)) => FunctorialAction (act :: Action)

instance FunctorialAction Any
instance FunctorialAction Base
instance FunctorialAction Atom
instance FunctorialAction AtomicMove
instance FunctorialAction Fail
instance FunctorialAction Move
instance FunctorialAction Query
instance FunctorialAction Sure
instance FunctorialAction SureBase
instance FunctorialAction SureQuery
