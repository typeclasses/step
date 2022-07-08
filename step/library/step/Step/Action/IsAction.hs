module Step.Action.IsAction where

import Step.Internal.Prelude

import Step.Action.Functor
import Step.Action.Join
import Step.Action.Types
import Step.Action.KindJoin

type IsAction (k :: ActionKind) =
    ( FunctorAction k
    , ActionJoin k SureQuery
    , ActionJoin SureQuery k
    , k :> SureQuery ~ k
    , SureQuery :> k ~ k
    )
