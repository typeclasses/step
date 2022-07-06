module Step.Action.UnifiedType where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Step.Action.SeparateTypes as T

import Step.Action.Functor (FunctorAction)

import qualified Monad

import qualified Step.Action.CoercedJoin as CJ

import Step.Action.KindJoin

import Step.Action.Join

class AlwaysMoves (k :: ActionKind)
instance AlwaysMoves T.Move
instance AlwaysMoves T.MoveUndo
instance AlwaysMoves T.SureMove

---

class Noncommittal (k :: ActionKind)
  where
    type Try k :: ActionKind
    try :: Functor m => k config cursor error m a -> (Try k) config cursor error m (Maybe a)

instance Noncommittal T.Undo
  where
    type Try T.Undo = T.Sure
    try = Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any

instance Noncommittal T.MoveUndo
  where
    type Try T.MoveUndo = T.SureMove
    try = Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any

instance Noncommittal T.Static
  where
    type Try T.Static = T.SureStatic
    try = Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any

---

type IsAction k =
    ( SureStaticId k
    , FunctorAction k
    )

---

type SureStaticId k =
    ( ActionJoin k T.SureStatic
    , ActionJoin T.SureStatic k
    , k :> T.SureStatic ~ k
    , T.SureStatic :> k ~ k
    )
