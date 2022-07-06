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

class CanFail (k :: ActionKind)
  where
    failure :: Monad m => (config -> StateT cursor m error) -> k config cursor error m a

instance CanFail T.Any      where failure = view Coerce.coerced . T.failureAny
instance CanFail T.Static   where failure = view Coerce.coerced . T.failureAny
instance CanFail T.Move     where failure = view Coerce.coerced . T.failureAny
instance CanFail T.Undo     where failure = view Coerce.coerced . T.failureAny
instance CanFail T.MoveUndo where failure = view Coerce.coerced . T.failureAny

---

type IsAction k =
    ( SureStaticId k
    , FunctorAction k
    )

---

class
    ( ActionJoin k T.SureStatic
    , ActionJoin T.SureStatic k
    , k :> T.SureStatic ~ k
    , T.SureStatic :> k ~ k
    )
    => SureStaticId k

instance SureStaticId T.Any
instance SureStaticId T.Static
instance SureStaticId T.Move
instance SureStaticId T.Undo
instance SureStaticId T.MoveUndo
instance SureStaticId T.Sure
instance SureStaticId T.SureStatic
instance SureStaticId T.SureMove

---

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => k1 config cursor error m (k2 config cursor error m a) -> (k1 :> k2) config cursor error m a

-- todo: all 64 instances

instance ActionJoin T.Sure       T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.Move       T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveUndo   T.MoveUndo   where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Sure       T.SureMove   where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.MoveUndo   T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.MoveUndo   where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveUndo   T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.MoveUndo   where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Undo       where actionJoin = CJ.anyToAny
instance ActionJoin T.Undo       T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Static     where actionJoin = CJ.anyToAny
instance ActionJoin T.Static     T.Move       where actionJoin = CJ.anyToAny

instance ActionJoin T.Any        T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Any        where actionJoin = CJ.sureToAny
instance ActionJoin T.Static     T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Static     where actionJoin = CJ.sureToAny
instance ActionJoin T.Move       T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Move       where actionJoin = CJ.sureToAny
instance ActionJoin T.Undo       T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Undo       where actionJoin = CJ.sureToAny
instance ActionJoin T.MoveUndo   T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.MoveUndo   where actionJoin = CJ.sureToAny
instance ActionJoin T.Sure       T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.SureMove   where actionJoin = CJ.sureToSure
