module Step.Action.IsAction where

import Step.Internal.Prelude

import Step.Action.Functor
import Step.Action.Join
import Step.Action.Kinds
import Step.Action.KindJoin

type IsAction (k :: ActionKind) =
    ( FunctorAction k
    , ActionJoin k SureStatic
    , ActionJoin SureStatic k
    , k :> SureStatic ~ k
    , SureStatic :> k ~ k
    )
