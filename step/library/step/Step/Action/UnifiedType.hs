module Step.Action.UnifiedType where

import Step.Internal.Prelude

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import qualified Step.Action.Coerce as Coerce

import Step.Action.Functor (FunctorAction)

import Step.Action.KindJoin

import Step.Action.Join

import Step.Action.Kinds

---

---

type IsAction k =
    ( FunctorAction k
    , ActionJoin k T.SureStatic
    , ActionJoin T.SureStatic k
    , k :> T.SureStatic ~ k
    , T.SureStatic :> k ~ k
    )
