{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, Safe, TypeFamilies, TypeOperators #-}

module Step.Action.Functorial where

import Step.Internal.Prelude

import Step.Action.Types

class (forall config cursor error m. Functor m =>
      Functor (k config cursor error m)) => FunctorialAction (k :: ActionKind)

instance FunctorialAction Any
instance FunctorialAction Query
instance FunctorialAction Move
instance FunctorialAction Atom
instance FunctorialAction AtomicMove
instance FunctorialAction Sure
instance FunctorialAction SureQuery
