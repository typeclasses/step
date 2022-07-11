{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, Safe, TypeFamilies, TypeOperators #-}

module Step.ActionTypes.Functorial where

import Step.Internal.Prelude

import Step.ActionTypes.Types

class (forall cursor error m. Functor m =>
      Functor (k cursor error m)) => FunctorialAction (k :: ActionKind)

instance FunctorialAction Any
instance FunctorialAction Atom
instance FunctorialAction AtomicMove
instance FunctorialAction Fail
instance FunctorialAction Move
instance FunctorialAction Query
instance FunctorialAction Sure
instance FunctorialAction SureQuery
