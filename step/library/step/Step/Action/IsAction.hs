{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, Safe, TypeFamilies, TypeOperators #-}

module Step.Action.IsAction where

import Step.Internal.Prelude

import Step.Action.Functor
import Step.Action.Join
import Step.Action.Types
import Step.Action.KindJoin

class
    ( FunctorAction k
    , ActionJoin k SureQuery
    , ActionJoin SureQuery k
    , k :> SureQuery ~ k
    , SureQuery :> k ~ k
    ) =>
    IsAction (k :: ActionKind)

instance IsAction Any
instance IsAction Query
instance IsAction Move
instance IsAction Atom
instance IsAction AtomicMove
instance IsAction Sure
instance IsAction SureQuery
