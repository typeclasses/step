{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, Safe, TypeFamilies, TypeOperators #-}

module Step.Action.IsAction where

import Step.Internal.Prelude

import Step.Action.Join
import Step.Action.Types
import Step.Action.KindJoin

class
    ( forall config cursor error m. Functor m =>
          Functor (k config cursor error m)
    , ActionJoin k SureQuery
    , ActionJoin SureQuery k
    ) =>
    IsAction (k :: ActionKind)

instance IsAction Any
instance IsAction Query
instance IsAction Move
instance IsAction Atom
instance IsAction AtomicMove
instance IsAction Sure
instance IsAction SureQuery
